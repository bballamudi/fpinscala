package fpinscala.datastructures


sealed trait List[+A] // `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing] // A `List` data constructor representing the empty list
/* Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`,
which may be `Nil` or another `Cons`.
 */
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List { // `List` companion object. Contains functions for creating and working with lists.
  def sum(ints: List[Int]): Int = ints match { // A function that uses pattern matching to add up a list of integers
    case Nil => 0 // The sum of the empty list is 0.
    case Cons(x,xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  val x = List(1,2,3,4,5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101
  }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h,t) => Cons(h, append(t, a2))
    }


  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x,y) => x + y)

  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar


  def tail[A](l: List[A]): List[A] =
    l match {
      case Cons(h, t) => t
      case Nil => Nil
  }

  def setHead[A](l: List[A], h: A): List[A] =
    l match {
      case Cons(_, t) => Cons(h, t)
      case Nil => Nil
    }

  def drop[A](l: List[A], n: Int): List[A] = {
    @annotation.tailrec
    def go(l: List[A], n: Int): List[A] = {
      if (n == 0) l
      else l match {
        case Cons(h, t) => go(t, n-1)
        case Nil => Nil
      }
    }
    go(l, n)
  }

  def dropWhile[A](l: List[A], f: A => Boolean): List[A]  = {
    @annotation.tailrec
    def go(l: List[A]): List[A] = {
      l match {
        case Cons(h, t) => if (f(h)) go(t) else l
        case Nil => Nil
      }
    }
    go(l)
  }



  def init[A](l: List[A]): List[A] = {

    def go(l: List[A]): List[A] = {
      l match {
        case Nil => Nil
        case Cons(h, Nil) => Nil
        case Cons(h, t) => Cons(h, go(t))
      }
    }
    go(l)
  }

  def length[A](l: List[A]): Int = foldRight(l, 0)((a: A, b: Int) => b + 1)

  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  @annotation.tailrec
  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B = {
    l match {
      case Nil => z
      case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
    }
  }

  def product3(xs: List[Int]) = foldLeft(xs, 1)((a: Int, b: Int) => a * b)
  def length2[A](xs: List[A]) = foldLeft(xs, 0)((a: Int, b: A) => a + 1)

  def reverse[A](xs: List[A]) = foldLeft(xs, List[A]())((accumulator: List[A], b: A) => Cons(b, accumulator))
  def foldLeftViaFoldRight[A, B](l: List[A], z: B)(f: (B, A) => B): B = {
    foldRight(l, (b: B) => b)((a,g) => b => g(f(b,a)))(z)
  }

  def foldRightViaFoldLeft[A, B](as: List[A], z: B)(f: (A, B) => B): B = {
    foldLeft(reverse(as), z)((a, b) => f(b, a))
  }

  def append2[A](a1: List[A], a2: List[A]): List[A] = {
    foldRight(a1, a2)((a: A, b: List[A]) => Cons(a, b))
  }

  // Exercise 3.15
  def appendL[A](ls: List[List[A]]) = foldLeft(ls, List[A]())(append2)

  // Exercise 3.16
  def addOne(as: List[Int]) = foldRight(as, Nil:List[Int])((h, t) => Cons(h+1, t))

  // Exercise 3.17
  def dToString(as: List[Double]) = foldRight(as, Nil:List[String])((h, t) => Cons(h.toString, t))

  // Exercise 3.18
  def map[A,B](l: List[A])(f: A => B): List[B] = foldRight(l, Nil:List[B])((h, t) => Cons(f(h), t))

  // Exercise 3.19
  def filter[A](l: List[A])(f: A => Boolean): List[A] = {
    foldRight(l, Nil:List[A])((h, t) => if (f(h)) Cons(h, t) else t)
  }

  // Exercise 3.20
  def flatMap[A,B](l: List[A])(f: A => List[B]): List[B] = appendL(map(l)(f))

  // Exercise 3.21
  def filter2[A](l: List[A])(f: A => Boolean): List[A] = flatMap(l)(i => if (f(i)) List(i) else List())

  // Exercise 3.22
  def addLists(xs: List[Int], ys: List[Int]): List[Int] = 
    (xs, ys) match {
      case (Nil, _) => Nil
      case (_, Nil) => Nil
      case (Cons(xh, xt), Cons(yh, yt)) => Cons(xh + yh, addLists(xt, yt))
    }

  // Exercise 3.23
  def zipWith[A, B, C](as: List[A], bs: List[B])(f: (A, B) => C): List[C] = 
    (as, bs) match {
      case (Nil, _) => Nil
      case (_, Nil) => Nil
      case (Cons(ah, at), Cons(bh, bt)) => Cons(f(ah, bh), zipWith(at, bt)(f))
    }

  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = 
    (sup, sub) match {
      case (_, Nil) => true
      case (Nil, _) => false
      case (Cons(supH, supT), Cons(subH, subT)) => if (supH == subH) hasSubsequence(supT, subT) else hasSubsequence(supT, sub)
    }
}

object ChapterThree {
  import List._

  def main(args: Array[String]) {
    val xs = Cons(1, Cons(2, Cons(3, Cons(4, Nil))))
    val xs2 = Cons(3, Cons(4, Nil))

    assert(Cons(55, Cons(2, xs2)) == setHead(xs, 55))
    assert(Cons(2, xs2) == tail(xs))
    assert(xs2 == drop(xs, 2))
    assert(xs2 == dropWhile(xs, (x: Int) => x < 3))
    assert(init(xs) == Cons(1, Cons(2, Cons(3, Nil))))
    assert(length(xs) == 4)

    assert(foldLeft(xs, 0)((a: Int, b: Int) => a + b) == 10)
    assert(product3(xs) == 24)
    assert(length2(xs) == 4)

    assert(append2(Cons(1, Cons(2, Nil)), Cons(3, Cons(4, Nil))) == xs)
    assert(appendL(Cons(xs, Cons(xs2, Nil))) == Cons(1, Cons(2,
      Cons(3, Cons(4, Cons(3, Cons(4, Nil)))))))

    assert(addOne(xs2) == Cons(4, Cons(5, Nil))) // 3.16
    assert(dToString(Cons(3.0, Cons(4.0, Nil))) == Cons("3.0", Cons("4.0", Nil))) // 3.17
    assert(map(xs2)((x) => x + 1) == Cons(4, Cons(5, Nil))) // 3.18
    assert(filter(xs)(x => x % 2 == 0) == Cons(2, Cons(4, Nil))) // 3.19
    assert(filter(xs)(x => x % 2 == 1) == Cons(1, Cons(3, Nil))) // 3.19


    assert(flatMap(List(1, 2, 3))(i => List(i, i)) == List(1, 1, 2, 2, 3, 3)) // 3.20
    assert(filter2(xs)(x => x % 2 == 1) == Cons(1, Cons(3, Nil))) // 3.21

    assert(addLists(List(1, 2, 3), List(4, 5, 6)) == List(5, 7, 9)) // 3.22
    assert(zipWith(List(1, 2, 3), List(4, 5, 6))(_+_) == List(5, 7, 9)) // 3.23

    assert(hasSubsequence(List(1, 2, 3, 4), List(1, 2))) // 3.24
    assert(hasSubsequence(List(1, 2, 3, 4), List(2, 3))) // 3.24
    assert(hasSubsequence(List(1, 2, 3, 4), List(4))) // 3.24

    //
    // Trees
    //
    import Tree._
    val t = Branch(Branch(Leaf("a"), Leaf("b")), Branch(Leaf("c"), Leaf("d")))
    val t2 = Branch(Branch(Leaf(1), Leaf(3)), Branch(Leaf(5), Leaf(7)))
    val t3 = Branch(Branch(Leaf(2), Leaf(4)), Branch(Leaf(6), Leaf(8)))

    assert(size(t) == 4) // 3.25
    assert(maximum(t2) == 7) // 3.26
    assert(depth(t2) == 3) // 3.27
    assert(tMap(t2)(x => x+1) == t3)

    assert(size(t) == size2(t))
    assert(maximum(t2) == maximum2(t2))
    assert(depth(t2) == depth2(t2))
    assert(tMap(t2)(x => x + 1) == tMap2(t2)(x => x + 1))
  }
}
