package fpinscala.datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {
  
  // Exercise 3.25
  def size[A](t: Tree[A]): Int =
    t match {
      case Leaf(v) => 1
      case Branch(l, r) => size(l) + size(r)
    }

  // Exercise 3.26
  def maximum(t: Tree[Int]): Int = 
    t match {
      case Leaf(v) => v
      case Branch(l, r) => maximum(l) max maximum(r)
    }

  // Exercise 3.27
  def depth[A](t: Tree[A]): Int =
    t match {
      case Leaf(_) => 1
      case Branch(l, r) => (depth(l) max depth(r)) + 1
    }

  // exercise 3.28
  def tMap[A, B](t: Tree[A])(f: A => B): Tree[B] = 
    t match {
      case Leaf(v) => Leaf(f(v))
      case Branch(l, r) => Branch(tMap(l)(f), tMap(r)(f))
    }

  // exercise 3.29
  def tFold[A, B](t: Tree[A])(f: A => B)(f2: (B, B) => B): B = 
    t match {
      case Leaf(v) => f(v)
      case Branch(l, r) => f2(tFold(l)(f)(f2), tFold(r)(f)(f2))
    }

  def size2[A](t: Tree[A]) = tFold(t)(_ => 1)((l, r) => l + r)
  def maximum2(t: Tree[Int]) = tFold[Int, Int](t)((v: Int) => v)((l, r) => l max r)
  def depth2[A](t: Tree[A]) = tFold(t)((v: A) => 1)((l, r) => (l max r) + 1)
  def tMap2[A, B](t: Tree[A])(f: A => B) = tFold[A, Tree[B]](t)(v => Leaf(f(v)))((l, r) => Branch[B](l, r))
}
