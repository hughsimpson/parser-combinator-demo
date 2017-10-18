package parser_combinator_demo

import java.io.InputStream

object Utils {
  private def slurpInputStream(is: InputStream) = scala.io.Source.fromInputStream(is).getLines.mkString("\n")
  def slurpRsc(s: String) = slurpInputStream(getClass.getResourceAsStream(s"/$s"))
}