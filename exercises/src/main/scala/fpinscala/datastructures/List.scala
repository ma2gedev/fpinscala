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

  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x,y) => x + y)

  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar


  def tail[A](l: List[A]): List[A] = {
    l match {
      case Nil => l
      case Cons(x, xs) => xs
    }
  }

  def setHead[A](l: List[A], h: A): List[A] = {
    l match {
      case Nil => Cons(h, l)
      case Cons(x, xs) => Cons(h, xs)
    }
  }

  def drop[A](l: List[A], n: Int): List[A] = {
    @annotation.tailrec
    def go(list: List[A], n: Int): List[A] = {
      if (n == 0) list
      else list match {
        case Nil => list
        case Cons(x, xs) => go(xs, n - 1)
      }
    }
    go(l, n)
  }

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = {
    @annotation.tailrec
    def go(list: List[A], f: A => Boolean): List[A] = {
      list match {
        case Nil => list
        case Cons(x, xs) => if (f(x)) go(xs, f)
          else list
      }
    }
    go(l, f)
  }

  def init[A](l: List[A]): List[A] = {
    //@annotation.tailrec
    def go(list: List[A]): List[A] = {
      list match {
        case Nil => list
        case Cons(x, Nil) => Nil
        case Cons(x, xs) => Cons(x, go(xs))
      }
    }
    go(l)
  }

  def length[A](l: List[A]): Int = {
    foldRight(l, 0)((_, y) => 1 + y)
  }

  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B = {
    l match {
      case Nil => z
      case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
    }
  }

  def sum_foldLeft(ns: List[Int]) =
    foldLeft(ns, 0)((x, y) => x + y)

  def product_foldLeft(ns: List[Double]) =
    foldLeft(ns, 1.0)((x, y) => x * y)

  def length_foldLeft[A](l: List[A]): Int = {
    foldLeft(l, 0)((x, _) => x + 1)
  }

  def reverse[A](l: List[A]): List[A] = {
    foldLeft(l, List[A]())((acc, h) => Cons(h, acc))
  }

  def foldLeftWithFoldRight[A,B](l: List[A], z: B)(f: (B, A) => B): B = {
    foldRight(l, z)((a, b) => f(b, a))
  }

  def foldRightWithFoldLeft[A,B](l: List[A], z: B)(f: (A, B) => B): B = {
    foldLeft(l, z)((b, a) => f(a, b))
  }

  def appendWithFoldRight[A](a1: List[A], a2: List[A]): List[A] = {
    foldRight(a1, a2)((x, y) => Cons(x, y))
  }

  def concat[A](list: List[List[A]]): List[A] = {
    foldRight(list, Nil:List[A])(append)
  }

  def add1(list: List[Int]): List[Int] = {
    foldRight(list, Nil:List[Int])((x, acc) => Cons(x + 1, acc))
  }

  def doubleToString(list: List[Double]): List[String] = {
    foldRight(list, Nil:List[String])((x, acc) => Cons(x.toString, acc))
  }

  def map[A,B](l: List[A])(f: A => B): List[B] = {
    foldRight(l, Nil:List[B])((x, acc) => Cons(f(x), acc))
  }

  def filter[A](as: List[A])(f: A => Boolean): List[A] = {
    foldRight(as, Nil:List[A])((x, acc) =>
      if (f(x)) Cons(x, acc)
      else acc
    )
  }

  def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] = {
    concat(map(as)(f))
  }

  def filterWithFlatMap[A](as: List[A])(f: A => Boolean): List[A] = {
    flatMap(as)(x => if (f(x)) List(x) else Nil)
  }

  def zipAdd(list1: List[Int], list2: List[Int]): List[Int] = {
    (list1, list2) match {
      // compiler shows the following:
      // [warn] It would fail on the following inputs: (Cons(_, _), Nil), (Nil, Cons(_, _)), (Nil, Nil)
      // [warn]     (list1, list2) match {
      // [warn]     ^
      // [warn] one warning found
      case (Nil, _) => Nil
      case (_, Nil) => Nil
      case (Cons(h1, t1), Cons(h2, t2)) => Cons(h1 + h2, zipAdd(t1, t2))
    }
  }

  def zipWith[A, B, C](list1: List[A], list2: List[B])(f: (A, B) => C): List[C] = {
    (list1, list2) match {
      case (Nil, _) => Nil
      case (_, Nil) => Nil
      case (Cons(h1, t1), Cons(h2, t2)) => Cons(f(h1, h2), zipWith(t1, t2)(f))
    }
  }

  def contains[A](list: List[A], sub: List[A]): Boolean = {
    (list, sub) match {
      case (_, Nil) => true
      case (Cons(h1, t1), Cons(h2, t2)) if h1 == h2 => contains(t1, t2)
      case _ => false
    }
  }

  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = {
    (sup, sub) match {
      case (Nil, Nil) => true
      case (Nil, _) => false
      case (x, y) if (contains(x, y)) => true
      case (Cons(xh, xt), y) => hasSubsequence(xt, y)
    }
  }
}
