package fpinscala.datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {
  def size[A](tree: Tree[A]): Int = {
    tree match {
      case Leaf(value) => 1
      case Branch(l, r) => 1 + size(l) + size(r)
    }
  }

  def maximum(tree: Tree[Int]): Int = {
    tree match {
      case Leaf(value) => value
      case Branch(l, r) => maximum(l) max maximum(r)
    }
  }

  def depth[A](tree: Tree[A]): Int = {
    tree match {
      case Leaf(value) => 0
      case Branch(l, r) => 1 + (depth(l) max depth(r))
    }
  }

  def map[A,B](tree: Tree[A])(f: A => B): Tree[B] = {
    tree match {
      case Leaf(value) => Leaf(f(value))
      case Branch(l, r) => Branch(map(l)(f), map(r)(f))
    }
  }

  def fold[A,B](tree: Tree[A])(f: A => B)(g: (B, B) => B): B = {
    tree match {
      case Leaf(value) => f(value)
      case Branch(l, r) => g(fold(l)(f)(g), fold(r)(f)(g))
    }
  }

  def sizeWithFold[A](tree: Tree[A]): Int = fold(tree)((x) => 1)((x,y) => 1 + x + y)
  def maximumWithFold(tree: Tree[Int]): Int = fold(tree)((x) => x)((x,y) => x max y)
  def depthWithFold[A](tree: Tree[A]): Int = fold(tree)((x) => 0)((x,y) => 1 + (x max y))
  def mapWithFold[A,B](tree: Tree[A])(f: A => B): Tree[B] = fold(tree)((x) => Leaf(f(x)): Tree[B])((x, y) => Branch(x, y))
  // add `Tree[B]` into `((x) => Leaf(f(x)): Tree[B])` to avoid the following error:
  // [error]  found   : fpinscala.datastructures.Branch[B]
  // [error]  required: fpinscala.datastructures.Leaf[B]
  // [error]   def mapWithFold[A,B](tree: Tree[A])(f: A => B): Tree[B] = fold(tree)((x) => Leaf(f(x)))((x, y) => Branch(x, y))
  // [error]                                                                                                           ^
  // [error] one error found
}
