package parser_combinator_demo.components

trait BooleanParser extends BaseParsers {
  // Constants
  lazy val fAnd = literal("&&") ^^^ AndCond
  lazy val fOr = literal("||") ^^^ OrCond
  lazy val fNot = (literal("!") | literal("¬")) ^^^ NotCond
  lazy val condCombinator: Parser[BinaryCond] = fAnd | fOr

  def wrappedCond[T](condLeaf: Parser[BooleanNode[T]]): Parser[BooleanNode[T]] = condLeaf | `(..)`(cond(condLeaf))
  def cond[T](condLeaf: Parser[BooleanNode[T]]): Parser[BooleanNode[T]] =
    (wrappedCond(condLeaf) ~^~ (condCombinator ~^~ wrappedCond(condLeaf)).*) ^^ {
      case a ~ rest if rest.isEmpty => a
      case a ~ rest =>
        val ops = rest.map{ case op ~ c => (op, c) }
        assert(ops.map(_._1).distinct.size == 1)
        BinaryNode(ops.head._1, a +: ops.map(_._2))
    }

  def unaryWrappedLeaf[T](condLeaf: Parser[BooleanNode[T]]) =
    condLeaf |
    (fNot ~^~ wrappedCond(condLeaf)) ^^ {case op ~ c => UnaryNode(op, c)}
  def condWithNot[I] = (x:Parser[BooleanNode[I]]) => cond(unaryWrappedLeaf(x))

}
