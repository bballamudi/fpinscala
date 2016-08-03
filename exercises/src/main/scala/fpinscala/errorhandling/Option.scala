package fpinscala.errorhandling


import scala.{Option => _, Some => _, Either => _, _} // hide std library `Option`, `Some` and `Either`, since we are writing our own in this chapter

sealed trait Option[+A] {

  // Exercise 4.1
  def map[B](f: A => B): Option[B] = 
    this match {
      case None => None
      case Some(v) => Some(f(v))
    }

  def getOrElse[B>:A](default: => B): B =
    this match {
      case None => default
      case Some(v) => v
    }

  def flatMap[B](f: A => Option[B]): Option[B] = 
    this match {
      case None => None
      case Some(v) => f(v)
    }

  def orElse[B>:A](ob: => Option[B]): Option[B] =
    this match {
      case None => ob
      case Some(_) => this
    }

  def filter(f: A => Boolean): Option[A] = 
    this match {
      case None => None
      case Some(v) => if (f(v)) this else None
    }
}
case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

object Option {
  def failingFn(i: Int): Int = {
    val y: Int = throw new Exception("fail!") // `val y: Int = ...` declares `y` as having type `Int`, and sets it equal to the right hand side of the `=`.
    try {
      val x = 42 + 5
      x + y
    }
    catch { case e: Exception => 43 } // A `catch` block is just a pattern matching block like the ones we've seen. `case e: Exception` is a pattern that matches any `Exception`, and it binds this value to the identifier `e`. The match returns the value 43.
  }

  def failingFn2(i: Int): Int = {
    try {
      val x = 42 + 5
      x + ((throw new Exception("fail!")): Int) // A thrown Exception can be given any type; here we're annotating it with the type `Int`
    }
    catch { case e: Exception => 43 }
  }

  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)

  // Exercise 4.2
  def variance(xs: Seq[Double]): Option[Double] = mean(xs).flatMap(mu => mean(xs.map(x => Math.pow(x - mu, 2))))

  // Exercise 4.3
  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = 
    (a, b) match {
      case (None, _) => None
      case (_, None) => None
      case (Some(x), Some(y)) => Some(f(x, y))
    }

  // Exercise 4.4
  def sequence[A](a: List[Option[A]]): Option[List[A]] = 
    a match {
      case Nil => Some(Nil)
      case h :: t => h.flatMap(head => sequence(t).map(head :: _))
    }

  // Exercise 4.5
  // I want to take a list of A and a fn of A => Option[B] and receive an Option[List[B]]
  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] =
    a match {
      case Nil => Some(Nil)
      case h::t => map2(f(h), traverse(t)(f))(_ :: _)
    }
}

object ChapterFour {

  import Option._

  def main(args: Array[String]): Unit = {

    // 4.1
    assert(Some(3).map(x => x + 1) == Some(4))
    assert(Some(3).getOrElse(4) == 3)
    assert(None.getOrElse(4) == 4)
    assert(Some(3).flatMap(x => Some(x + 4)) == Some(7))
    assert(Some(3).orElse(Some(4)) == Some(3))
    assert(None.orElse(Some(4)) == Some(4))
    assert(Some(3).filter(x => x < 5) == Some(3))
    assert(Some(3).filter(x => x > 5) == None)

    assert(1 == 1) // 4.2

    // 4.3
    val f = (x: Int, y: Int) => x + y
    assert(map2(None, None)(f) == None)
    assert(map2(Some(3), None)(f) == None)
    assert(map2(None, Some(3))(f) == None)
    assert(map2(Some(3), Some(4))(f) == Some(7))

    // 4.4
    val l1 = List(Some(1), Some(2), Some(3))
    val l2 = List(Some(1), None, Some(3))
    assert(sequence(l1) == Some(List(1, 2, 3)))
    assert(sequence(l2) == None)
  }
}
