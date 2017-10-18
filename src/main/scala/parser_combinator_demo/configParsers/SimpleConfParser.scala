package parser_combinator_demo.configParsers

import scala.concurrent.duration._

import parser_combinator_demo.Utils._
import parser_combinator_demo.components.BaseParsers

case class Service(url: String = "", timeout: Duration = 10 seconds)
case class SimpleConf(port: Int = 0, service1: Service = Service())

object SimpleConfParser extends BaseParsers {
  val serviceParser: Parser[Service] = `{..}`(
    (literal("url")     ~^> sep ~^> s ) ~^~
    (literal("timeout") ~^> sep ~^> bareWord)
  ) ^^ {
    case url ~ timeout => Service(url, Duration.create(timeout))
  }

  val configParseStage: Parser[SimpleConf => SimpleConf] =
    literal("service1") ~^> sep ~^> commit(serviceParser) ^^ { x => (_:SimpleConf).copy(service1 = x)} |
    literal("port")     ~^> sep ~^> commit(int)           ^^ { x => (_:SimpleConf).copy(port = x)}

  val configParser: Parser[SimpleConf] = configParseStage.* ^^ { _.foldLeft(SimpleConf()){ case (p, pf) => pf(p) } }

  def parseConfig(s: String): SimpleConf = parse(configParser, slurpRsc(s)) match {
    case Success(cp, _) => cp
    case NoSuccess(f, in) => throw new RuntimeException(s"parse failure: $f @ line ${in.pos.line}, col ${in.pos.column}")
  }
}