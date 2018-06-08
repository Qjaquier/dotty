package dotty.tools.dotc
package coverage

import ast.tpd._
import dotty.tools.dotc.core.Contexts.Context

/**
 *
 * Adapted from the class Location of SCoverage 1.4.0
 *
 * @param packageName the name of the enclosing package
 * @param className the name of the closest enclosing class
 * @param fullClassName the fully qualified name of the closest enclosing class
 */
case class Location(packageName: String,
                    className: String,
                    fullClassName: String,
                    classType: String,
                    method: String,
                    sourcePath: String) extends java.io.Serializable

object Location {

  def apply(tree: Tree)(implicit ctx: Context) : Location = {
    val packageName = ctx.owner.denot.enclosingPackageClass.name.toString()
    val className = ctx.owner.denot.enclosingClass.name.toString()


    Location(packageName,
             className,
             packageName+"."+className,
             "Class", //TODO
             ctx.owner.denot.enclosingMethod.name.toString(),
             ctx.source.file.absolute.toString())
  }
}