package parser_combinator_demo

import scala.collection.immutable.ListMap

import org.scalatest.{FreeSpec, Matchers}
import parser_combinator_demo.components.{AndCond, BinaryNode, BooleanLeaf, NotCond, OrCond, UnaryNode}
import parser_combinator_demo.configParsers.{BooleanConfig, BooleanConfigParser, Funct, Functs}

class BooleanParserTest extends FreeSpec with Matchers {
  "boolean config parsing" in {
    val model1 = Functs[String](Map(
      "ns1" -> Funct(ListMap(
        BinaryNode(AndCond,List(BooleanLeaf("var1"), BinaryNode(OrCond,List(BooleanLeaf("var2"), BooleanLeaf("var3"))))) -> "met condition 1",
        BinaryNode(AndCond,List(UnaryNode(NotCond,BooleanLeaf("var1")), UnaryNode(NotCond,BooleanLeaf("var2")), UnaryNode(NotCond,BooleanLeaf("var3")))) -> "met condition 2",
        BooleanLeaf("default") -> "fallback")),
      "ns2" -> Funct(ListMap(
        BinaryNode(AndCond,List(BinaryNode(OrCond,List(BooleanLeaf("var1"), BooleanLeaf("var2"))), BinaryNode(OrCond,List(BooleanLeaf("var3"), UnaryNode(NotCond,BooleanLeaf("var4")))))) -> "met condition 1",
        BooleanLeaf("default") -> "fallback"))))

    val model2 = Functs[(String, String)](Map(
      "ns1" -> Funct(ListMap(
        BooleanLeaf("foo","baz") -> "%2$s :: %1$s",
        BinaryNode(AndCond,List(UnaryNode(NotCond,BooleanLeaf("foo","bar")), BooleanLeaf("bar","default"))) -> "%2$s ::: %1$s",
        BooleanLeaf("default","default") -> "fallback[%s, %s]",
        BooleanLeaf("default","failed") -> "FAIL: %s, %s")),
      "ns2" -> Funct(ListMap(
        BooleanLeaf("^.*baz","quux") -> "met condition 1",
        BooleanLeaf("default","failed") -> "FAIL",
        BooleanLeaf("default","default") -> "fallback"))))

    BooleanConfigParser.parseConfig("binary.conf") shouldEqual BooleanConfig("YES", "NO", model1, model2)
  }

  "evaluate some boolean expressions with variables" - {
    val logicModel = BooleanConfigParser.parseConfig("binary.conf").myLogicModel
    val defaultIsTrue = "default" -> true
    "case 1" in {
      val vars = Map("var1" -> true, "var2" -> true, "var3" -> false, "var4" -> false)
      logicModel("ns1"){ vars + defaultIsTrue }.get shouldEqual "met condition 1"
    }
    "case 2" in {
      val vars = Map("var1" -> false, "var2" -> false, "var3" -> false, "var4" -> false)
      logicModel("ns1"){ vars + defaultIsTrue }.get shouldEqual "met condition 2"
    }
    "case 3" in {
      val vars = Map("var1" -> false, "var2" -> true, "var3" -> true, "var4" -> false)
      logicModel("ns1"){ vars + defaultIsTrue }.get shouldEqual "fallback"
    }
  }

  "evaluate some more complex boolean expressions with variables" - {
    val logicModel = BooleanConfigParser.parseConfig("binary.conf").myMoreComplexLogicModel
    def meetsConditions(in: (String, String))(pair: (String, String)) =
      Set(in._1, "default").contains(pair._1) && !Set(in._2).contains(pair._2)
    def fmt(in: (String, String))(x: String) = String.format(x, in._1, in._2)
    "case 1" in {
      val in = ("foo", "20")
      logicModel("ns1"){ meetsConditions(in) }.map(fmt(in)).get shouldEqual "20 :: foo"
    }
    "case 2" in {
      val in = ("bar", "30")
      logicModel("ns1"){ meetsConditions(in) }.map(fmt(in)).get shouldEqual "30 ::: bar"
    }
    "case 3" in {
      val in = ("foobar", "40")
      logicModel("ns1"){ meetsConditions(in) }.map(fmt(in)).get shouldEqual "fallback[foobar, 40]"
    }
    "case 4" in {
      val in = ("bar", "default")
      logicModel("ns1"){ meetsConditions(in) }.map(fmt(in)).get shouldEqual "FAIL: bar, default"
    }
  }

}