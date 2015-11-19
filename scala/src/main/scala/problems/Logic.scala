package problems
/*
 * 99 Problems, 46-48.
 * Logic
 *
 * No using builtins.
 */

object Logic46 {
  sealed abstract     class Bool
  case object True  extends Bool
  case object False extends Bool

  def and (a: Bool, b: Bool) = (a,b) match {
    case (True, True) => True
    case _ => False
  }

  def or (a: Bool, b: Bool) = (a,b) match {
    case (False, False) => False
    case _ => True
  }

  def not (a: Bool) = a match {
    case True => False
    case False => True
  }

  def nand (a: Bool, b: Bool) = not(and(a,b))
  def nor  (a: Bool, b: Bool) = not(or(a,b))
  def equ  (a: Bool, b: Bool) = or(and(a,b), not(or(a,b)))
  def xor  (a: Bool, b: Bool) = not(equ(a,b))
  def impl (a: Bool, b: Bool) = not(and(a,not(b)))

  def table(f: (Bool, Bool) => Bool) = {
    printf("%-6s %-6s %-10s\n", "A", "B", "result")
    for { a <- List(True, False)
        ; b <- List(True, False) } {
      printf("%-6s %-6s %-6s\n", a, b, f(a,b))
    }
  }
}

// Make Bool an object and not an algebraic type
object Logic47 {
  sealed abstract class Bool {
  }
  case object True  extends Bool
  case object False extends Bool

  def and (a: Bool, b: Bool): Bool = (a,b) match {
    case (True, True) => True
    case _ => False
  }

  def or (a: Bool, b: Bool): Bool = (a,b) match {
    case (False, False) => False
    case _ => True
  }

  def not (a: Bool): Bool = a match {
    case True => False
    case False => True
  }

  def nand (a: Bool, b: Bool): Bool = not(and(a,b))
  def nor  (a: Bool, b: Bool): Bool = not(or(a,b))
  def equ  (a: Bool, b: Bool): Bool = or(and(a,b), and(not(a),not(b)))
  def xor  (a: Bool, b: Bool): Bool = not(equ(a,b))
  def impl (a: Bool, b: Bool): Bool = not(and(a,not(b)))
}
