package dotty.tools
package dottydoc

import dotc.core.Contexts
import Contexts.{ Context, ContextBase, FreshContext }
import dotc.util.SourceFile
import dotc.core.Phases.Phase
import dotc.typer.FrontEnd
import dottydoc.core.DocASTPhase

trait DottyTest {
  dotty.tools.dotc.parsing.Scanners // initialize keywords

  implicit var ctx: FreshContext = {
    val base = new ContextBase
    import base.settings._
    val ctx = base.initialCtx.fresh
    ctx.setSetting(ctx.settings.language, List("Scala2"))
    ctx.setSetting(ctx.settings.YkeepComments, true)
    base.initialize()(ctx)
    ctx
  }

  private def compilerWithChecker(assertion: DocASTPhase => Unit) = new DocCompiler {
    private[this] val docPhase = new DocASTPhase

    override def phases =
      List(new FrontEnd) ::
      List(docPhase) ::
      List(new Phase {
        def phaseName = "assertionPhase"
        override def run(implicit ctx: Context): Unit = assertion(docPhase)
      }) ::
      Nil
  }

  def checkSource(source: String)(assertion: DocASTPhase => Unit): Unit = {
    val c = compilerWithChecker(assertion)
    c.rootContext(ctx)
    val run = c.newRun
    run.compile(source)
  }

  def checkFiles(sources: List[String])(assertion: DocASTPhase => Unit): Unit = {
    val c = compilerWithChecker(assertion)
    c.rootContext(ctx)
    val run = c.newRun
    run.compile(sources)
  }

  def checkSources(sourceFiles: List[SourceFile])(assertion: DocASTPhase => Unit): Unit = {
    val c = compilerWithChecker(assertion)
    c.rootContext(ctx)
    val run = c.newRun
    run.compileSources(sourceFiles)
  }
}
