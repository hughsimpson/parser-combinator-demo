package parser_combinator_demo

import scala.collection.immutable.ListMap

import org.scalatest.{FreeSpec, Matchers}
import parser_combinator_demo.components.{AndCond, BinaryNode, BooleanLeaf1, BooleanLeaf2, NotCond, OrCond, UnaryNode}
import parser_combinator_demo.configParsers.{BooleanConfig, BooleanConfigParser, Funct, Functs}

class BooleanParserTest extends FreeSpec with Matchers {
  "boolean config parsing" in {
    val model1 = Functs[String](Map(
      "ns1" -> Funct(ListMap(
        BinaryNode(AndCond,List(BooleanLeaf1("var1"), BinaryNode(OrCond,List(BooleanLeaf1("var2"), BooleanLeaf1("var3"))))) -> "met condition 1",
        BinaryNode(AndCond,List(UnaryNode(NotCond,BooleanLeaf1("var1")), UnaryNode(NotCond,BooleanLeaf1("var2")), UnaryNode(NotCond,BooleanLeaf1("var3")))) -> "met condition 2",
        BooleanLeaf1("default") -> "fallback")),
      "ns2" -> Funct(ListMap(
        BinaryNode(AndCond,List(BinaryNode(OrCond,List(BooleanLeaf1("var1"), BooleanLeaf1("var2"))), BinaryNode(OrCond,List(BooleanLeaf1("var3"), UnaryNode(NotCond,BooleanLeaf1("var4")))))) -> "met condition 1",
        BooleanLeaf1("default") -> "fallback"))))

    val model2 = Functs[(String, String)](Map(
      "ns1" -> Funct(ListMap(
        BooleanLeaf2("foo","baz") -> "%2$s :: %1$s",
        BinaryNode(AndCond,List(UnaryNode(NotCond,BooleanLeaf2("foo","bar")), BooleanLeaf2("bar","default"))) -> "%2$s ::: %1$s",
        BooleanLeaf2("default","default") -> "fallback[%s, %s]",
        BooleanLeaf2("default","failed") -> "FAIL: %s, %s")),
      "ns2" -> Funct(ListMap(
        BooleanLeaf2("^.*baz","quux") -> "met condition 1",
        BooleanLeaf2("default","failed") -> "FAIL",
        BooleanLeaf2("default","default") -> "fallback"))))

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
      val in = ("foo", "twenty")
      logicModel("ns1"){ meetsConditions(in) }.map(fmt(in)).get shouldEqual "twenty :: foo"
    }
    "case 2" in {
      val in = ("bar", "thirty")
      logicModel("ns1"){ meetsConditions(in) }.map(fmt(in)).get shouldEqual "thirty ::: bar"
    }
    "case 3" in {
      val in = ("foobar", "forty")
      logicModel("ns1"){ meetsConditions(in) }.map(fmt(in)).get shouldEqual "fallback[foobar, forty]"
    }
    "case 4" in {
      val in = ("bar", "default")
      logicModel("ns1"){ meetsConditions(in) }.map(fmt(in)).get shouldEqual "FAIL: bar, default"
    }
  }

}