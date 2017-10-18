package parser_combinator_demo

import scala.collection.immutable.ListMap

import org.scalatest.{FreeSpec, Matchers}
import parser_combinator_demo.components.{AndCond, BinaryNode, BooleanLeaf1, BooleanLeaf2, NotCond, OrCond, UnaryNode}
import parser_combinator_demo.configParsers.{BooleanConfig, BooleanConfigParser, Funct, Functs}

class BooleanParserTest extends FreeSpec with Matchers {
  "boolean config parsing" in {
    val model1 = Functs[String](Map(
      "iri1" -> Funct(ListMap(
        BinaryNode(AndCond,List(BooleanLeaf1("foo"), BinaryNode(OrCond,List(BooleanLeaf1("bar"), BooleanLeaf1("baz"))))) -> "met condition 1",
        BinaryNode(AndCond,List(UnaryNode(NotCond,BooleanLeaf1("foo")), UnaryNode(NotCond,BooleanLeaf1("bar")), UnaryNode(NotCond,BooleanLeaf1("baz")))) -> "met condition 2",
        BooleanLeaf1("default") -> "fallback")),
      "iri2" -> Funct(ListMap(
        BinaryNode(AndCond,List(BinaryNode(OrCond,List(BooleanLeaf1("foo"), BooleanLeaf1("bar"))), BinaryNode(OrCond,List(BooleanLeaf1("baz"), UnaryNode(NotCond,BooleanLeaf1("quux")))))) -> "met condition 1",
        BooleanLeaf1("default") -> "fallback"))))

    val model2 = Functs[(String, String)](Map(
      "iri1" -> Funct(ListMap(
        BooleanLeaf2("foo","baz") -> "met condition 1",
        BinaryNode(AndCond,List(UnaryNode(NotCond,BooleanLeaf2("foo","bar")), BooleanLeaf2("bar","default"))) -> "met condition 2",
        BooleanLeaf2("default","default") -> "fallback")),
      "iri2" -> Funct(ListMap(
        BooleanLeaf2("^.*baz","quux") -> "met condition 1",
        BooleanLeaf2("default","default") -> "fallback"))))

    BooleanConfigParser.parseConfig("binary.conf") shouldEqual BooleanConfig("YES", "NO", model1, model2)
  }

  "evaluate something" in {
    val binaryConf = BooleanConfigParser.parseConfig("binary.conf")
    binaryConf.myLogicModel("iri1")(s => "foobar".contains(s) || s == "default") shouldEqual Some("met condition 1")
    binaryConf.myLogicModel("iri1")(s => "fiddledeedee".contains(s) || s == "default") shouldEqual Some("met condition 2")
    binaryConf.myLogicModel("iri1")(s => "barbaz".contains(s) || s == "default") shouldEqual Some("fallback")
  }

}