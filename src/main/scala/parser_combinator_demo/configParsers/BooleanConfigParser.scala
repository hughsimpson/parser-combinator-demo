package parser_combinator_demo.configParsers

import scala.collection.immutable.ListMap

import parser_combinator_demo.Utils.slurpRsc
import parser_combinator_demo.components.{BooleanLeaf1, BooleanLeaf2, BooleanNode, BooleanParser}

case class Funct[T](fs: ListMap[BooleanNode[T], String]) {
  def apply(t: T => Boolean): Option[String] = fs collectFirst { case (k, v) if k.eval(t) => v }
}
case class Functs[T](fs: Map[String, Funct[T]])  { def apply(s: String) = fs(s) }

case class BooleanConfig(`true`: String = "true",
                         `false`: String = "false",
                         myLogicModel: Functs[String] = Functs(Map()),
                         myMoreComplexLogicModel: Functs[(String, String)] = Functs(Map()))


object BooleanConfigParser extends BooleanParser {
  lazy val boolLeaf1: Parser[BooleanLeaf1] = s ^^ { BooleanLeaf1 }
  lazy val boolLeaf2: Parser[BooleanLeaf2] =
    s ~^~ `(..)`(bareWord) ^^ { case pos ~ elem => BooleanLeaf2(pos, elem) } |
    s ^^ { BooleanLeaf2(_, default) }

  lazy val boolStruct1: Parser[BooleanNode[String]] = condWithNot(boolLeaf1)
  lazy val boolStruct2: Parser[BooleanNode[(String, String)]] = condWithNot(boolLeaf2)

  def modelEntry[T](p: Parser[BooleanNode[T]]): Parser[Funct[T]] = okvs(p, s) ^^ { Funct(_) }
  def modelParser[T](p: Parser[BooleanNode[T]]): Parser[Functs[T]] = kvs(s, modelEntry(p)) ^^ { Functs(_) }

  private lazy val configParseStage =
    literal("true")                    ~^> sep ~^> commit(s)                         ^^ { x => (_:BooleanConfig).copy(`true` = x) } |
    literal("false")                   ~^> sep ~^> commit(s)                         ^^ { x => (_:BooleanConfig).copy(`false` = x) } |
    literal("myLogicModel")            ~^> sep ~^> commit(modelParser(boolStruct1))  ^^ { x => (_:BooleanConfig).copy(myLogicModel = x) } |
    literal("myMoreComplexLogicModel") ~^> sep ~^> commit(modelParser(boolStruct2))  ^^ { x => (_:BooleanConfig).copy(myMoreComplexLogicModel = x) }

  val configParser: Parser[BooleanConfig] = configParseStage.* ^^ { _.foldLeft(BooleanConfig()){ case (p, pf) => pf(p) } }

  def parseConfig(s: String): BooleanConfig = parse(configParser, slurpRsc(s)) match {
    case Success(cp, _) => cp
    case NoSuccess(f, in) => throw new RuntimeException(s"parse failure: $f @ line ${in.pos.line}, col ${in.pos.column}")
  }
}