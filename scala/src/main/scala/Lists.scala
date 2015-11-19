/*
 * 99 Problems, 1-28.
 * Lists
 */

object Lists {

  def last[A](xs: List[A]): A = xs match {
    case l::Nil => l
    case h::t => last(t)
    case _ => throw new Exception ("Empty list")
  }

  def penultimate[A](xs: List[A]): A = xs match {
    case p::_::Nil => p
    case h::t => penultimate(t)
    case _ => throw new Exception ("Not enough elems")
  }

  def nth[A](n: Int, xs: List[A]): A =
    if (n == 0) { xs.head }
    else { nth(n-1, xs.tail) }

  def length[A](xs: List[A]): Int = xs match {
    case Nil => 0
    case h::t => 1 + length(t)
  }

  def reverse[A](xs: List[A]): List[A] = {
    def go[A](acc: List[A], xs: List[A]): List[A] = xs match {
      case Nil => acc
      case h::t => go (h::acc, t)
    }
    go(List(), xs)
  }

  def isPalindrome[A](xs: List[A]): Boolean =
    xs == reverse(xs)

  def flatten[A](xs: List[List[A]]): List[A] = xs flatMap identity

  def compress

}
