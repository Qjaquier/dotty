package dotty.tools.dotc
package coverage

import java.io._

import scala.io.Source
/**
 * Adapted from the class Serializer of SCoverage 1.4.0
 */
object Serializer {

  val CoverageFileName = "scoverage.coverage.xml"
  // Write out coverage data to the given data directory, using the default coverage filename
  def serialize(coverage: Coverage, dataDir: String): Unit = serialize(coverage, coverageFile(dataDir))

  // Write out coverage data to given file.
  def serialize(coverage: Coverage, file: File): Unit = {
    val writer = new BufferedWriter(new FileWriter(file))
    serialize(coverage, writer)
    writer.close()
  }

  def serialize(coverage: Coverage, writer: Writer): Unit = {
    def writeStatement(stmt: Statement, writer: Writer): Unit = {
      writer.write{
        "<statement>" +
          "<source>" +
            stmt.source +
          "</source>" +
          "<package>" +
           escape(stmt.location.packageName) +
          "</package>" +
          "<class>" +
            escape(stmt.location.className) +
          "</class>" +
          "<classType>"+
            escape(stmt.location.classType.toString)+
          "</classType>"+
          "<fullClassName>"+
            escape(stmt.location.fullClassName)+
          "</fullClassName>"+
          "<method>"+
            escape(stmt.location.method) +
         "</method>"+
          "<path>"+
            stmt.location.sourcePath+
          "</path>"+
          "<id>"+
            stmt.id.toString+
          "</id>"+
          "<start>"+
            stmt.start.toString+
          "</start>"+
          "<end>"+
            stmt.end.toString+
          "</end>"+
          "<line>"+
            stmt.line.toString+
          "</line>"+
          "<description>"+
            escape(stmt.desc)+
          "</description>"+
          "<symbolName>"+
            escape(stmt.symbolName)+
          "</symbolName>"+
          "<treeName>"+
            escape(stmt.treeName)+
          "</treeName>"+
          "<branch>"+
            stmt.branch.toString+
          "</branch>"+
          "<count>"+
            stmt.count.toString+
          "</count>"+
          "<ignored>"+
            stmt.ignored.toString+
          "</ignored>"+
        "</statement>"+
        "\n"
      }
    }
    writer.write("<statements>\n")
    coverage.statements.foreach(stmt => writeStatement(stmt, writer))
    writer.write("</statements>")
  }

  def coverageFile(dataDir: File): File = coverageFile(dataDir.getAbsolutePath)
  def coverageFile(dataDir: String): File = new File(dataDir, CoverageFileName)

  /**
   * This method ensures that the output String has only
   * valid XML unicode characters as specified by the
   * XML 1.0 standard. For reference, please see
   * <a href="http://www.w3.org/TR/2000/REC-xml-20001006#NT-Char">the
   * standard</a>. This method will return an empty
   * String if the input is null or empty.
   *
   * @param in The String whose non-valid characters we want to remove.
   * @return The in String, stripped of non-valid characters.
   * @see http://blog.mark-mclaren.info/2007/02/invalid-xml-characters-when-valid-utf8_5873.html
   *
   */
  def escape(in: String): String = {
    val out = new StringBuilder()
    for ( current <- Option(in).getOrElse("").toCharArray ) {
      if ((current == 0x9) || (current == 0xA) || (current == 0xD) ||
        ((current >= 0x20) && (current <= 0xD7FF)) ||
        ((current >= 0xE000) && (current <= 0xFFFD)) ||
        ((current >= 0x10000) && (current <= 0x10FFFF))){
          if(!(current == '>' || current == '<' || current == '&'))
            out.append(current)
        }
    }
    out.mkString
  }
}
