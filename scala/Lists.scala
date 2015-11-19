object Lists {
  def last[A](xs: List[A]): A = xs match {
    case l::Nil => l
    case h::t => last(t)
    case _ => throw new Exception ("Empty list")
  }



}
