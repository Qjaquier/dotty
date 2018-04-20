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
import java.io.File


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
    override def transform(tree: Tree)(implicit ctx: Context): Tree = {
        tree match {
          case tree:If =>
            //This is instrumentation for branch coverage, the thenp and elesp will be instrumented twice
            //but only counted once in the statement coverage percentage.
            //It seems not possible to instrument it once due to nested if that will not be correctly handled.
            cpy.If(tree)(thenp = instrument(super.transform(tree.thenp), true), elsep = instrument(super.transform(tree.elsep), true))
          // case tree : ValDef =>
          //   if(tree.pos.isSourceDerived){
          //     cpy.ValDef(tree)(rhs = instrument(tree.rhs))
          //   } else{
          //     tree
          //   }
          case tree : Apply =>
            super.transform(tree)
          case tree : Match =>
            //Don't instrument the selector
            cpy.Match(tree)(tree.selector, instrumentCases(tree.cases))
          case tree : Try =>
            //Instrument try and finally part as branch
            cpy.Try(tree)(expr = instrument(tree.expr, true), cases  = instrumentCases(tree.cases), finalizer = instrument(tree.finalizer, true))
          case tree : Select =>
            //TODO : Instrument the name ?
            if(tree.pos.isSourceDerived){
              cpy.Select(tree)(instrument(tree.qualifier), tree.name)
            } else {
              tree
            }

          case tree : CaseDef =>
            instrumentCaseDef(tree)
          case tree:Literal =>
            instrument(super.transform(tree))
          case tree:Ident =>
                if(tree.isType || !tree.pos.isSourceDerived){// || tree.isValue){
                  //Don't instrument a type
                  tree
                } else {
                  instrument(super.transform(tree))
                }

          case _ => super.transform(tree)
        }
    }

    def instrumentCases(cases : List[CaseDef])(implicit ctx: Context): List[CaseDef] = {
      cases.map{instrumentCaseDef}
    }

    def instrumentCaseDef(tree : CaseDef)(implicit ctx: Context) : CaseDef = {
      //Don't add coverage to the pattern
      cpy.CaseDef(tree)(tree.pat, super.transform(tree.guard), super.transform(tree.body))
    }

    def instrument(tree : Tree, branch: Boolean = false)(implicit ctx: Context) : Tree = {
      if(tree.pos.exists){// && tree.pos.isSourceDerived){ //
        val id = statementId.incrementAndGet()
        val statement = new Statement(
          ctx.source.file.absolute.toString(), //Source
          Location(tree),
          id, //id
          tree.pos.start, //start
          tree.pos.end, //end
          ctx.source.offsetToLine(tree.pos.point), //line number
          tree.toString(), //Description
          tree.symbol.toString(), //symbol name
          tree.toString(),//Tree name
          branch //Branch
        )

        coverage.addStatement(statement)

        Block(List(invokeCall(id)), super.transform(tree))
      } else {
        //No position, nothing to instrument
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

