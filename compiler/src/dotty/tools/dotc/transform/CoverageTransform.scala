package dotty.tools.dotc
package transform

import MegaPhase._
import ast.Trees._
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
import dotty.tools.dotc.typer.Lifter
import java.io._


class CoverageTransform extends MiniPhase with IdentityDenotTransformer { thisPhase =>
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

  override def transformIf(tree: If)(implicit ctx: Context) = {
    //This is instrumentation for branch coverage, the thenp and elesp will be instrumented twice
    //but only counted once in the statement coverage percentage.
    //It seems not possible to instrument it once due to nested if that will not be correctly handled.
    cpy.If(tree)(thenp = instrument(tree.thenp, true), elsep = instrument(tree.elsep, true))
  }

  override def transformSelect(tree: Select)(implicit ctx: Context) = {
     println("Qualif")
     println(tree.qualifier.show)
    println(tree.qualifier.isPattern)
    println(tree.qualifier.isType)
    println(tree.qualifier.isValue)
    // println("end")
    //instrument(tree)
    //tree
    //if(tree.qualifier.isValue){
    //  tree
    //} else {
      cpy.Select(tree)(instrument(tree.qualifier), tree.name)
    //}
  }

//TODO : probleme here
  override def transformIdent(tree: Ident)(implicit ctx: Context) = {
    // println()
    // println("Ident")
    // println(tree.show)
    // println(tree.isPattern)
    // println(tree.isType)
    // println(tree.isValue)

    if(tree.isType || tree.isValue){
      //Don't instrument a type
      tree
    } else {
      instrument(tree)
    }
  }

   override def transformLiteral(tree: Literal)(implicit ctx: Context) = {
     instrument(tree)
   }

//
 override def transformMatch(tree: Match)(implicit ctx: Context) = {
    //Instrument the selector and the cases one by one
    instrumentCases(tree.cases)
    tree
    //cpy.Match(tree)(selector = instrument(tree.selector), cases = instrumentCases(tree.cases))
  }

  // override def transformTry(tree: Try)(implicit ctx: Context) = {
  //   //Instrument the three parts separately
  //   cpy.Try(tree)(expr = instrument(tree.expr), cases = instrumentCases(tree.cases), finalizer = instrument(tree.finalizer))
  // }

   override def transformApply(tree: Apply)(implicit ctx: Context) = {
     //print(tree.show)
     tree
    //  cpy.Apply(tree)(fun = instrument(tree.fun), args = tree.args)
   }
  // override def transformValDef(tree: ValDef)(implicit ctx: Context) = {
  //   //TODO : Unforced rhs ?
  //   println(i"valdef: $tree")
  //   cpy.ValDef(tree)(name = tree.name, tpt = tree.tpt, rhs = instrument(tree.rhs))
  // }

//  override def transformDefDef(tree: DefDef)(implicit ctx: Context) = {
//    cpy.DefDef(tree)(name = tree.name, tparams = tree.tparams, vparamss: List[List[ValDef]] = tree.vparamss, tpt = tree.tpt, rhs= tree.unforcedRhs)
//  }

//  override def transformBlock(tree: Block)(implicit ctx: Context) = {
//    instrument(cpy.Block(tree)(stats = tree.stats, expr= tree.expr))
//  }

//  override def transformSelect(tree: Select)(implicit ctx: Context) = {
//    instrument(tree)
//  }

//  override def transformLiteral(tree: Literal)(implicit ctx: Context) = {
//    instrument(tree)
//  }

  def instrumentCases(cases : List[CaseDef])(implicit ctx: Context): List[CaseDef] = {
    //No need to instrument the guard,

    //cases.foreach{ c => println(c.pat.show);println(c.pat.isValue) }
    cases
    //cases.map { case c => cpy.CaseDef(c)(c.pat, c.guard, instrument(c.body)) }
  }

  def instrument(tree : Tree, branch: Boolean = false)(implicit ctx: Context) : Tree = {
    if(tree.pos.exists && tree.pos.isSourceDerived){
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

      Block(List(invokeCall(id)), tree)
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