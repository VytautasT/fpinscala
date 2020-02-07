package fpinscala.laziness

import Stream._
trait Stream[+A] {

  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match {
      case Cons(h,t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z
    }

  def exists(p: A => Boolean): Boolean = 
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the stream. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }

  def toList(): List[A] = this match {
    case Empty => Nil
    case Cons(h, t) => h() :: t().toList()
  }

  def toList2(): List[A] =
    foldRight(List[A]())(_ :: _)

  def toList3(): List[A] = {
    @annotation.tailrec
    def go(acc: Stream[A], l: List[A]): List[A] = acc match {
      case Empty => l
      case Cons(h, t) => go(t(), h() :: l)
    }
    go(this, Nil) reverse
  }

  def take(n: Int): Stream[A] = {
    @annotation.tailrec
    def go(acc: Stream[A], s: Stream[A], n: Int): Stream[A] = s match {
      case Cons(h, t) if n > 1 => go(cons(h(), acc), t(), n - 1)
      case Cons(h, _) if n == 1 => cons(h(), acc)
      case _ => acc
    }
    go(empty, go(empty, this, n), n)
    //apply(go(empty, this, n).toList3.reverse: _*) it works too!
  }

  @annotation.tailrec
  final def drop(n: Int): Stream[A] = this match {
    case Cons(_, t) if n > 0 => t().drop(n - 1)
    case _ => this
  }

  def takeWhile(p: A => Boolean): Stream[A] = {
    @annotation.tailrec
    def go(acc: Stream[A], s: Stream[A]): Stream[A] = s match {
      case Cons(h, t) if p(h()) => go(cons(h(), acc), t())
      case _ => acc
    }
    apply(go(empty, this).toList3.reverse: _*)
  }

  def forAll(p: A => Boolean): Boolean =
    foldRight(true)(p(_) && _)

  def takeWhile2(p: A => Boolean): Stream[A] =
    foldRight(empty[A])((x, acc) => if (p(x)) cons(x, acc) else acc)

  def headOption: Option[A] =
    foldRight[Option[A]](None)((x, _) => {Some(x)})

  // 5.7 map, filter, append, flatmap using foldRight. Part of the exercise is
  // writing your own function signatures.
  def map[B](f: A => B): Stream[B] =
    foldRight(empty[B])((x, acc) => cons(f(x), acc))

  def filter(p: A => Boolean): Stream[A] =
    foldRight(empty[A])((x, acc) => if (p(x)) cons(x, acc) else acc)

  def append[AA >: A](s: => Stream[AA]): Stream[AA] =
    foldRight(s)(cons(_, _))

  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight(empty[B])(f(_) append _)

  def map2[B](f: A => B): Stream[B] =
    unfold(this){
      case Cons(h, t) => Some(f(h()), t())
      case _ => None
    }

  def take2(n: Int): Stream[A] =
    unfold(this, n) {
      case (Cons(h, t), 1) => Some(h(), (empty, 0))
      case (Cons(h, t), n) => Some(h(), (t(), n - 1))
      case _ => None
    }

  def takeWhile3(p: A => Boolean): Stream[A] =
    unfold(this){
      case Cons(h, t) if p(h()) => Some(h(), t())
      case _ => None
    }

  def zipWith[B, C](r: Stream[B])(f: (A, B) => C): Stream[C] =
    unfold(this, r){
      case (Cons(lh, lt), Cons(rh, rt)) => Some(f(lh(), rh()), (lt(), rt()))
      case _ => None
    }

  def zipAll[B](r: Stream[B]): Stream[(Option[A], Option[B])] =
    unfold(this, r){
      case (Cons(lh, lt), Cons(rh, rt)) => Some((Some(lh()), Some(rh())), (lt(), rt()))
      case (Cons(lh, lt), e) => Some((Some(lh()), None), (lt(), e))
      case (e, Cons(rh, rt)) => Some((None, Some(rh())), (e, rt()))
      case _ => None
    }

  def startsWith[B](s: Stream[B]): Boolean =
    zipAll(s)
      .takeWhile(_._2.isDefined)
      .forAll { case (l, r) =>
        (for {ll <- l; rr <- r} yield ll == rr) getOrElse(false) }


  def tails: Stream[Stream[A]] =
    cons(this, unfold(this) {
      case Empty => None
      case Cons(_, t) => Some(t(), t())
    })

  def tails2: Stream[Stream[A]] = // also good
    unfold(this) {
      case Empty => None
      case s => Some((s, s drop 1))
    } append Stream(empty)

  def hasSubsequence[A](s: Stream[A]): Boolean =
    tails exists(_ startsWith s)

  def scanRight[B](z: => B)(f: (A, => B) => B): Stream[B] =
    foldRight(Stream(z))((x, acc) => {
      lazy val lazyAcc = acc
      cons(f(x, (lazyAcc headOption) getOrElse z), lazyAcc)
    })
}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty 
    else cons(as.head, apply(as.tail: _*))

  val ones: Stream[Int] = Stream.cons(1, ones)

  def constant[A](a: A): Stream[A] = {
    // This is more efficient than `cons(a, constant(a))` since it's just
    // one object referencing itself.
    // [Me]: Idk, maybe?
    lazy val tail: Stream[A] = Cons(() => a, () => tail)
    tail
  }

  def from(n: Int): Stream[Int] =
    cons(n, from(n + 1))

  val fibs: Stream[Int] = {
    def go(cur: => Int, next: => Int): Stream[Int] =
      cons(cur, go(next, cur + next))
    go(0, 1)
  }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] =
    f(z) map{ case (a, s) => cons(a, unfold(s)(f)) } getOrElse(empty)

  val fibs2 = unfold((0, 1))({
    case (f0, f1) => Some((f0, (f1, f0 + f1)))
  })

  def from2(n: Int): Stream[Int] =
    unfold(n)(s => Some((s, s + 1)))

  def constant2[A](a: A): Stream[A] =
    unfold(a)(_ => Some(a, a))

  val ones2: Stream[Int] =
    unfold(1)(_ => Some(1, 1))
}