package problems

/*
 * 99 Problems, 1-28.
 * Lists.
 *
 * The exercise is to use as few builtin functions as possible,
 * since we are re-implementing most of the standard list functions.
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
    if (n < 0) throw new IndexOutOfBoundsException("negative")
    else xs match {
      case Nil => throw new IndexOutOfBoundsException("empty")
      case x::xs => if (n == 0) x else nth(n-1, xs)
    }

  def length[A](xs: List[A]): Int = xs match {
    case Nil => 0
    case h::t => 1 + length(t)
  }

  def reverse[A](xs: List[A]) = {
    def go[A](acc: List[A], xs: List[A]): List[A] = xs match {
      case Nil => acc
      case h::t => go (h::acc, t)
    }
    go (List(), xs)
  }

  def isPalindrome[A](xs: List[A]): Boolean =
    xs == reverse(xs)

  def flatten[A](xss: List[List[A]]) = {
    def addAll[A](toAdd: List[A], acc: List[A]): List[A] = toAdd match {
      case Nil => acc
      case h::t => addAll(t, h::acc)
    }

    def go[A](acc: List[A], xss: List[List[A]]): List[A] = xss match {
      case Nil => reverse(acc)
      case xs::xss => go(addAll(xs,acc), xss)
    }

    go (List(), xss)
  }

  def compress[A](xs: List[A]) = {
    def go[A](acc: List[A], xs: List[A]): List[A] = xs match {
      case Nil => reverse(acc)
      case h::t => acc match {
        case Nil => go (h::acc,t)
        case a::as => if (h == a) go (acc,t) else go (h::acc, t)
      }
    }
    go (List(), xs)
  }

  def pack[A](xs: List[A]) = {
    def group[A](x: A, acc: List[List[A]]) = acc match {
      case Nil => List(x) :: acc
      case h::t => if (h.head == x) (x::h)::t else List(x) :: acc
    }

    xs.foldRight(List[List[A]]())(group)
  }

  def encode[A](xs: List[A]) = {
    pack(xs) map { g => (g.length, g.head) }
  }

  def encodeModified[A](xs: List[A]) = {
    encode(xs) map { case (n,c) => if (n == 1) c else (n,c) }
  }

  def decode[A](xs: List[(Int,A)]) = {
    def replicate[A](e: A, n: Int) =
      if (n <= 0) List() else e :: replicate(e,n-1)

    xs flatMap { case (n, c) => replicate(c, n)}
  }
}
