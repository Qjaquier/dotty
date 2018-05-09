package dotty.tools.dotc
package transform

import MegaPhase._
import ast.Trees._
import core._
import ast.untpd
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.DenotTransformers._
import dotty.tools.dotc.core.Constants.Constant
import java.util.concurrent.atomic.AtomicInteger
import dotty.tools.dotc.coverage.Coverage
import dotty.tools.dotc.coverage.Statement
import dotty.tools.dotc.coverage.Location
import dotty.tools.dotc.coverage.Serializer
import dotty.tools.dotc.core.Types._
import dotty.tools.dotc.core.Symbols._
import dotty.tools.dotc.core.Decorators._
import dotty.tools.dotc.typer.LiftCoverage
import java.io.File
import collection.mutable
import dotty.tools.dotc.util.Positions._


class CoverageTransformMacro extends MacroTransform with IdentityDenotTransformer { thisPhase =>
  import ast.tpd._

  override def phaseName = "coverage"

  val statementId = new AtomicInteger(0)

  var outputPath = ""

  //Main class used to store all intrumented statements
  val coverage = new Coverage()

  override def run(implicit ctx: Context): Unit = {
    //Test if -coverage is present, otherwise, don't add coverage probe.
    if (ctx.settings.coverageOutputDir.value != ""){
        outputPath = ctx.settings.coverageOutputDir.value
        super.run
        //Clean the outputpath dir
        val dataDir = new File(outputPath)
        dataDir.listFiles.filter(_.getName.startsWith("scoverage.")).foreach(_.delete)
        //Write to a file the data of the coverage
        Serializer.serialize(coverage, outputPath)
    }
  }

  protected def newTransformer(implicit ctx: Context): Transformer = new CoverageTransformer

  class CoverageTransformer extends Transformer {
    var instrumented = false

    override def transform(tree: Tree)(implicit ctx: Context): Tree = {
        tree match {
          case tree: If =>
            //This is instrumentation for branch coverage, the thenp and elesp will be instrumented twice
            //but only counted once in the statement coverage percentage.
            //It seems not possible to instrument it once due to nested if that will not be correctly handled.
            cpy.If(tree)(cond = transform(tree.cond), thenp = instrument(transform(tree.thenp), true), elsep = instrument(transform(tree.elsep), true))
          case tree: Try =>
            //Instrument try and finally part as branch
            cpy.Try(tree)(expr = instrument(transform(tree.expr), true), cases  = instrumentCases(tree.cases), finalizer = instrument(transform(tree.finalizer), true))
          case tree: Apply =>
           //TODO: do not lift boolean : tree.symbol.
            if(LiftCoverage.needLift(tree)){
              var buffer = mutable.ListBuffer[Tree]()
              //If only one of the args needs to be lifted, we have to lift everything
              val lifted = LiftCoverage.liftForCoverage(buffer,tree)
              //Instrument the new trees lifted
              val instrumented = buffer.toList.map(transform)
              //We can now instrument the apply as it is with a custom position to point to the function
              Block(instrumented, instrument(lifted, Position(tree.fun.pos.point,tree.fun.pos.end) ,false))
            } else {
              //No arguments to lift, continue the instrumentation on the subtree
              super.transform(tree)
            }
          case tree: Select =>
            if(tree.qualifier.isInstanceOf[New]){
              //New must be wrapped in a select, instrument the whole select
              instrument(tree)
            } else {
              cpy.Select(tree)(transform(tree.qualifier), tree.name)
            }
          case tree: CaseDef =>
            instrumentCaseDef(tree)
          //Instrument as it is
          case tree: Literal =>
            instrument(tree)
          case tree: Ident =>
            instrument(tree)
          case tree: New =>
            instrument(tree)
          case tree: This =>
            instrument(tree)
          case tree: Super =>
            instrument(tree)
          //Instrument partially
          case tree: PackageDef =>
            //Don't instrument the pid of the package but instrument the statements
            cpy.PackageDef(tree)(tree.pid, transform(tree.stats))
          case tree:Assign =>
            cpy.Assign(tree)(tree.lhs, transform(tree.rhs))
          //Don't instrument
          case tree: Import =>
            tree
          //Transform the rest
          case tree:Closure =>
            print(tree)
            tree
          case _ => super.transform(tree)
        }
    }

    def instrumentCases(cases : List[CaseDef])(implicit ctx: Context): List[CaseDef] = {
      cases.map{instrumentCaseDef}
    }

    def instrumentCaseDef(tree : CaseDef)(implicit ctx: Context) : CaseDef = {
      //Don't add coverage to the pattern // TODO : Check if not alreay done
      cpy.CaseDef(tree)(tree.pat, transform(tree.guard), transform(tree.body))
    }

    def instrument(tree : Tree, branch: Boolean = false)(implicit ctx: Context) : Tree = {
      instrument(tree, tree.pos, branch)
    }

    def instrument(tree : Tree, pos: Position,  branch: Boolean)(implicit ctx: Context) : Tree = {
      if(pos.exists && !pos.isZeroExtent && !tree.isType){// && !tree.symbol.is(Flags.PackageClass)
        val id = statementId.incrementAndGet()
        val statement = new Statement(
          ctx.source.file.absolute.toString(), //Source
          Location(tree),
          id, //id
          pos.start, //start
          pos.end, //end
          ctx.source.offsetToLine(pos.point), //line number
          tree.toString(), //Description
          tree.symbol.toString(), //symbol name
          tree.toString(),//Tree name
          branch //Branch
        )
        //Add the statement to the coverage data
        coverage.addStatement(statement)
        //Return the new tree with the instrumented tree
        Block(List(invokeCall(id)), tree)
      } else {
        //No position or the tree is a type, nothing to instrument
        tree
      }
    }

  /**
   * Invoke the runtime invoker with the id given.
   */
  def invokeCall(id: Int)(implicit ctx: Context) : Tree = {
     ref(defn.InvokerModuleRef).select("invoked".toTermName).appliedToArgs(List(Literal(Constant(id)), Literal(Constant(outputPath))))
  }

  }
}

