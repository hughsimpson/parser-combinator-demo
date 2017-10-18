package parser_combinator_demo

import org.scalatest.{FreeSpec, Matchers}
import parser_combinator_demo.configParsers.{BooleanConfigParser, Functs}

class ArabicTest extends FreeSpec with Matchers {

  def runTest(logicModel: Functs[(String, String)]) = {
    val logicModel = BooleanConfigParser.parseConfig("arabic.conf").myMoreComplexLogicModel
    def meetsConditions(in: (String, String))(pair: (String, String)) =
      Set(in._1, "default").contains(pair._1) && !Set(in._2).contains(pair._2)
    def fmt(in: (String, String))(x: String) = String.format(x, in._1, in._2)
    "case 1" in {
      val in = ("foo", "twenty")
      logicModel("ns1"){ meetsConditions(in) }.map(fmt(in)).get shouldEqual "twenty خدران في رجل foo"
    }
    "case 2" in {
      val in = ("bar", "thirty")
      logicModel("ns1"){ meetsConditions(in) }.map(fmt(in)).get shouldEqual "thirty خدران في رجل bar"
    }
    "case 3" in {
      val in = ("foobar", "forty")
      logicModel("ns1"){ meetsConditions(in) }.map(fmt(in)).get shouldEqual "خدران في رجل[foobar, forty]"
    }
    "case 4" in {
      val in = ("bar", "default")
      logicModel("ns1"){ meetsConditions(in) }.map(fmt(in)).get shouldEqual "خدران في رجل: bar, default"
    }
  }
  "evaluate some more complex boolean expressions with variables (arabic)" - {
    runTest(BooleanConfigParser.parseConfig("arabicUnreadable.conf").myMoreComplexLogicModel)
  }
  "evaluate some more complex boolean expressions with variables (arabic - modified)" - {
    runTest(BooleanConfigParser.parseConfig("arabic.conf").myMoreComplexLogicModel)
  }
}
