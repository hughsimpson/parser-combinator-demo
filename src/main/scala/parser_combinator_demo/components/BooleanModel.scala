package parser_combinator_demo.components

/**
  * our boolean logic model
  * */
// Operators
sealed trait CondType

sealed trait UnaryCond extends CondType
case object NotCond extends UnaryCond

sealed trait BinaryCond extends CondType
case object AndCond extends BinaryCond
case object OrCond extends BinaryCond

// Nodes
sealed trait BooleanNode[T] {
  def eval(fn: T => Boolean): Boolean
}
case class BooleanLeaf1(v: String) extends BooleanNode[String] {
  def eval(fn: String => Boolean): Boolean = fn(v)
}
case class BooleanLeaf2(p1: String, p2: String) extends BooleanNode[(String, String)] {
  def eval(fn: ((String, String)) => Boolean): Boolean = fn((p1, p2))
}
case class UnaryNode[T](condType: UnaryCond, cond: BooleanNode[T]) extends BooleanNode[T] {
  def eval(fn: T => Boolean): Boolean = {
    condType match {
      case NotCond => !cond.eval(fn)
    }
  }
}
case class BinaryNode[T](condType: BinaryCond, conds: Seq[BooleanNode[T]]) extends BooleanNode[T] {
  def eval(fn: T => Boolean): Boolean =
    condType match {
      case OrCond => conds.exists(_.eval(fn))
      case AndCond => conds.forall(_.eval(fn))
    }
}