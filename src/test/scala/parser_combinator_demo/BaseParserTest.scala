package parser_combinator_demo

import org.scalatest.{FreeSpec, Matchers}
import parser_combinator_demo.components.BaseParsers

class BaseParserTest extends FreeSpec with Matchers {
  val baseParser = new BaseParsers {}
  import baseParser.Success

  "string parsing" - {
    "parse a bareword" in {
      val in = "just_testing "
      baseParser.parse(baseParser.bareWord, in) match {
        case Success("just_testing", _) => succeed
        case _ => fail("bareword parsing failed")
      }
    }

    "parse a string" in {
      val in = """ "lzidjf#®•uy9w84منy!\"" """
      val expected = """lzidjf#®•uy9w84منy!""""
      baseParser.parse(baseParser.str, in) match {
        case Success(`expected`, _) => succeed
        case _ => fail("string parsing failed")
      }
    }
    "parse a url" in {
      val in = """"https://hostdomain.com/service1/v1""""
      val expected = "https://hostdomain.com/service1/v1"
      baseParser.parse(baseParser.str, in) match {
        case Success(`expected`, _) => succeed
        case _ => fail("string parsing failed")
      }
    }

    "parse an 's'" in {
      val bareword = "just_testing "
      val str =""" "lzidjf#®•uy9w84منy!\"" """
      baseParser.parse(baseParser.s, bareword) match {
        case Success("just_testing", _) => succeed
        case Success(x, _) => fail(s"string parsing failed [$x]")
      }
      baseParser.parse(baseParser.s, str) match {
        case Success("""lzidjf#®•uy9w84منy!"""", _) => succeed
        case _ => fail("string parsing failed")
      }
    }
  }

}
