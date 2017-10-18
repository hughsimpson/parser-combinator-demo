package parser_combinator_demo.components

import scala.collection.immutable.ListMap
import scala.util.parsing.combinator.{JavaTokenParsers, RegexParsers}

trait BaseParsers extends RegexParsers with JavaTokenParsers {
  // base primitives
  val bareWord: Parser[String] = """\w[\w\d]*""".r ^^ { _.toString }
  val str: Parser[String] = stringLiteral ^^ { s => s.substring(1, s.length-1)
    .replaceAll("""\\\"""", "\"")
    .replaceAll("""\\\\""", """\\""")
    .replaceAll("/([0-9]+)/", "%$1\\$s")
  }
  val s: Parser[String] = bareWord | str
  val int: Parser[Int] = bareWord ^^ { _.toInt }

  val comma: Parser[String] = literal(",")
  val sep: Parser[String] = literal(":") | literal("=")

  val comment: Parser[String] =
    ("/\\*"+"(?:[^*]|\\*[^/])*"+"\\*/").r |
    ("(?://|#)"+"[^\n]*"+"\n").r

  def `[..]`[T](p: Parser[T]): Parser[T] = literal("[") ~> p <~ literal("]")
  def `{..}`[T](p: Parser[T]): Parser[T] = literal("{") ~> p <~ literal("}")
  def `(..)`[T](p: Parser[T]): Parser[T] = literal("(") ~> p <~ literal(")")

  // higher level helpers
  implicit class RicherParser[T](val p: Parser[T]) {
    // like ~, <~ and ~> except that they allow comments anywhere in the produced parser, including before and after p&q
    def ~^~[U](q: => Parser[U]): Parser[T ~ U] = comment.* ~> (p <~ comment.*)  ~ q <~ comment.*
    def <^~[U](q: => Parser[U]): Parser[T]     = comment.* ~>  p <~ comment.*  <~ q <~ comment.*
    def ~^>[U](q: => Parser[U]): Parser[U]     = comment.* ~>  p ~> comment.*  ~> q <~ comment.*
  }

  def kv[K, V](kp: Parser[K], vp: Parser[V]): Parser[(K, V)] = kp ~^~ sep ~^~ vp <^~ comma.? ^^ {case a ~ _ ~ b => (a, b)}
  def kvs[K, V](kp: Parser[K], vp: Parser[V]): Parser[Map[K, V]] = `{..}`( kv(kp, vp).* ) ^^ { _.toMap }
  def okvs[K, V](kp: Parser[K], vp: Parser[V]): Parser[ListMap[K, V]] = `{..}`( kv(kp, vp).* ) ^^ { ListMap(_: _*) }
}
