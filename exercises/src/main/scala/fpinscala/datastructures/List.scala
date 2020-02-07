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


  def tail[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(_, xs) => xs
  }

  def setHead[A](l: List[A], h: A): List[A] = l match {
    case Nil => Nil
    case Cons(_, xs) => Cons(h, xs)
  }

  def drop[A](l: List[A], n: Int): List[A] = l match {
    case Cons(_, xs) if n > 0 => drop(xs, n - 1)
    case _ => l
  }

  def dropWhile[A](l: List[A])(f: A => Boolean): List[A] = l match {
    case Cons(x, xs) if f(x) => dropWhile(xs)(f)
    case _ => l
  }

  def init[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(_, Nil) => Nil
    case Cons(x, xs) => Cons(x, init(xs))
  }

  def length[A](l: List[A]): Int = foldRight(l, 0)((_, b) => b + 1)

  @annotation.tailrec
  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B = l match {
    case Nil => z
    case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
  }

//  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B = {
//    @annotation.tailrec
//    def go(acc: B, lst: List[A]): B = lst match {
//      case Nil => acc
//      case Cons(x, xs) => go(f(acc, x), xs)
//    }
//    go(z, l)
//  }

  def sum3(l: List[Int]): Int =
    foldLeft(l, 0)(_ + _)

  def product3(l: List[Double]): Double =
    foldLeft(l, 1.0)(_ * _)

  def length3[A](l: List[A]): Int =
    foldLeft(l, 0)((acc, _) => acc + 1)

  def reverse[A](l: List[A]): List[A] =
    foldLeft(l, Nil: List[A])((xs, x) => Cons(x, xs))

  def foldRight2[A, B](l: List[A], z: B)(f: (A, B) => B): B =
    foldLeft(reverse(l), z)((acc, a) => f(a, acc))

  def foldLeft2[A, B](l: List[A], z: B)(f: (B, A) => B): B =
    foldRight2(l, identity[B] _)((x, acc) => b => acc(f(b, x)))(z)

  def append2[A](l1: List[A], l2: List[A]): List[A] =
    foldRight2(l1, l2)(Cons(_, _))

  def concat[A](lists: List[List[A]]): List[A] =
    foldRight2(lists, List[A]())(append2)

  def mapAdd1(l: List[Int]): List[Int] =
    foldRight2(l, List[Int]())((x, acc) => Cons(x + 1, acc))

  def mapToString(l: List[Double]): List[String] =
    foldRight2(l, List[String]())((x, acc) => Cons(x.toString, acc))

  def map[A,B](l: List[A])(f: A => B): List[B] =
    foldRight2(l, List[B]())((x, acc) => Cons(f(x), acc))

  def filter[A](l: List[A])(p: A => Boolean): List[A] =
    foldRight2(l, List[A]())((x, acc) => if (p(x)) Cons(x, acc) else acc)

  def flatMap[A, B](l: List[A])(f: A => List[B]): List[B] =
    concat(map(l)(f))
    // alternatively: foldRight2(l, List[B]())((x, acc) => append2(f(x), acc))

  def filterFlatMap[A](l: List[A])(p: A => Boolean): List[A] =
    flatMap(l)(x => if (p(x)) List(x) else Nil)

  def addLists(l: List[Int], r: List[Int]): List[Int] = (l, r) match {
    case (Cons(x, xs), Cons(y, ys)) => Cons(x + y, addLists(xs, ys))
    case _ => Nil
  }

  def zipWith[A, B, C](l: List[A], r: List[B])(f: (A, B) => C): List[C] = (l, r) match {
    case (Cons(x, xs), Cons(y, ys)) => Cons(f(x, y), zipWith(xs, ys)(f))
    case _ => Nil
  }

  @annotation.tailrec
  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = {
    @annotation.tailrec
    def startsWith(sup: List[A], sub: List[A]): Boolean = (sup, sub) match {
      case (_, Nil) => true
      case (Nil, _) => false
      case (Cons(x, xs), Cons(y, ys)) => x == y && startsWith(xs, ys)
    }
    sup match {
      case Nil => sub == Nil
      case Cons(x, xs) => startsWith(sup, sub) || hasSubsequence(xs, sub)
    }
  }
}
