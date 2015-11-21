package specs

import org.scalacheck._
import Prop.forAll

// Here we test against default implementations
object ListsSpec extends Properties("List") {
  import problems.Lists._

  property("last") = forAll { l: List[Int] =>
    if (l.length >= 1) {
      last(l) == l.last
    } else {
      Prop.throws(classOf[Exception]) {  last(l)  }
    }
  }

  property("penultimate") = forAll { l: List[Int] =>
    if (l.length >= 2) {
      penultimate(l) == l.init.last
    } else {
      Prop.throws(classOf[Exception]) {  penultimate(l)  }
    }
  }

  property("nth") = forAll { (l: List[Int], n: Int) =>
    if (n < l.length && n >=0) {
      nth(n,l) == l(n)
    } else if (n < 0) {
      Prop.throws(classOf[IndexOutOfBoundsException]) {  nth(n,l)  }
    } else {
      Prop.throws(classOf[Exception]) {  nth(n,l)  }
    }
  }

  property("length") = forAll { l: List[Int] =>
    length(l) == l.length
  }

  property("reverse") = forAll { l: List[Int] =>
    reverse(l) == l.reverse
  }
}
