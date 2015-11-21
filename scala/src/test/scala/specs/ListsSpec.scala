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
    } else {
      Prop.throws(classOf[IndexOutOfBoundsException]) {  nth(n,l)  }
    }
  }

  property("length") = forAll { l: List[Int] =>
    length(l) == l.length
  }

  property("reverse") = forAll { l: List[Int] =>
    reverse(l) == l.reverse
  }

  property("flatten") = forAll { l: List[List[Int]] =>
    flatten(l) == l.flatten
  }

  property("compress") = forAll { l: List[Int] =>
    compress(l) == l.distinct
  }

  property("pack") = forAll { l: List[Int] =>
    l == pack(l).flatten
  }

//  property("encode") =  ???
}
