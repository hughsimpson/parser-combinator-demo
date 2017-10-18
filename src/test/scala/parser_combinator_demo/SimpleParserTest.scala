package parser_combinator_demo

import scala.concurrent.duration._

import org.scalatest.{FreeSpec, Matchers}
import parser_combinator_demo.configParsers.{Service, SimpleConf, SimpleConfParser}

class SimpleParserTest extends FreeSpec with Matchers {
  "simple config parsing" in {
    SimpleConfParser.parseConfig("simple.conf") shouldEqual SimpleConf(8080, Service("https://hostdomain.com/service1/v1", 1 second))
  }
  "can parse the config in a different order" in {
    SimpleConfParser.parseConfig("simpleReverse.conf") shouldEqual SimpleConf(8080, Service("https://hostdomain.com/service1/v1", 1 second))
  }
  "can include all the comments in our config" in {
    SimpleConfParser.parseConfig("comments.conf") shouldEqual SimpleConf(8080, Service("https://hostdomain.com/service1/v1", 1 second))
  }
  "gives us an appropriate exception when parsing fails" in {
    intercept[RuntimeException](SimpleConfParser.parseConfig("badSimple.conf")).getMessage shouldEqual
      "parse failure: `=' expected but `B' found @ line 4, col 6"
  }
}