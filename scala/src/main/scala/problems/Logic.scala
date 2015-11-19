package problems
/*
 * 99 Problems, 46-48.
 * Logic
 *
 * No using builtins.
 */

object Logic {
  abstract class Bool
  case object True  extends Bool
  case object False extends Bool

  def and(a: Bool, b: Bool): Bool = (a,b) match {
    case (True, True) => True
    case _ => False
  }

  def or(a: Bool, b: Bool): Bool = (a,b) match {
    case (False, False) => False
    case _ => True
  }

  def not(a: Bool): Bool = a match {
    case True => False
    case False => True
  }

  def nand(a: Bool, b: Bool): Bool = not(and(a,b))
  def nor(a: Bool, b: Bool): Bool = not(or(a,b))
  def equ(a: Bool, b: Bool): Bool = or(and(a,b), and(not(a),not(b)))
  def xor(a: Bool, b: Bool): Bool = not(equ(a,b))
  def impl(a: Bool, b: Bool): Bool = not(and(a,not(b)))

}
