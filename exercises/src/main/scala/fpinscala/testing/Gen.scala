package fpinscala.testing

import fpinscala.laziness.Stream
import fpinscala.state._
import fpinscala.parallelism._
import fpinscala.parallelism.Par.Par
import Gen._
import Prop._
import java.util.concurrent.{Executors,ExecutorService}

/*
The library developed in this chapter goes through several iterations. This file is just the
shell, which you can fill in and modify while working through the chapter.
*/


case class Prop(run: (MaxSize, TestCases, RNG) => Result) {
  def &&(p: Prop): Prop = Prop {
    (max, t, rng) => run(max, t, rng) match {
      case Passed => p.run(max, t, rng)
      case x => x
    }
  }

  def ||(p: Prop): Prop = Prop {
    (max, t, rng) => run(max, t, rng) match {
      case Falsified(fc1, _) => p.run(max, t, rng) match {
        case Falsified(fc2, sc2) => Falsified(fc1 + "\n"+ fc2, sc2)
        case x => x
      }
      case x => x
    }
  }
}

object Prop {
  type TestCases = Int
  type MaxSize = Int

  type FailedCase = String
  type SuccessCount = Int

  sealed trait Result {
    def isFalsified: Boolean
  }

  case object Proved extends Result {
    def isFalsified = false
  }

  case object Passed extends Result {
    def isFalsified = false
  }
  case class Falsified(failure: FailedCase,
                       successes: SuccessCount) extends Result {
    def isFalsified = true
  }

  def forAll[A](as: Gen[A])(f: A => Boolean): Prop = Prop {
    (_, n, rng) => randomStream(as)(rng).zipWith(Stream.from(0))((_, _)).take(n).map {
      case (a, i) => try {
        if (f(a)) Passed else Falsified(a.toString, i)
      } catch { case e: Exception => Falsified(buildMsg(a, e), i) }
    }.find(_.isFalsified).getOrElse(Passed)
  }

  def randomStream[A](g: Gen[A])(rng: RNG): Stream[A] =
    Stream.unfold(rng)(rng => Some(g.sample.run(rng)))


  def buildMsg[A](s: A, e: Exception): String =
    s"test case: $s\n" +
      s"generated an exception: ${e.getMessage}\n" +
      s"stack trace:\n ${e.getStackTrace.mkString("\n")}"

  def forAll[A](g: SGen[A])(f: A => Boolean): Prop =
    forAll(g(_))(f)

  def forAll[A](g: Int => Gen[A])(f: A => Boolean): Prop = Prop {
    (max, n, rng) =>
      val casesPerSize = (n + (max - 1)) / max
      val props: Stream[Prop] =
        Stream.from(0).take((n min max) + 1).map(i => forAll(g(i))(f))
      val prop: Prop =
        props.map(p => Prop { (max, _, rng) =>
          p.run(max, casesPerSize, rng)
        }).toList().reduce(_ && _)
      prop.run(max, n, rng)
  }

  val S = weighted(
    choose(1,4).map(Executors.newFixedThreadPool) -> .75,
    unit(Executors.newCachedThreadPool) -> .25
  )

  def forAllPar[A](g: Gen[A])(f: A => Par[Boolean]): Prop =
    forAll(S ** g) { case s ** a => f(a)(s).get }

  def equal[A](p: Par[A], p2: Par[A]): Par[Boolean] =
    Par.map2(p,p2)(_ == _)

  def check(p: => Boolean): Prop = Prop { (_, _, _) =>
    if (p) Proved else Falsified("()", 0)
  }

  def checkPar[A](p: => Par[Boolean]): Prop =
    forAllPar(Gen.unit(()))(_ => p)

  def run(p: Prop,
          maxSize: Int = 100,
          testCases: Int = 100,
          rng: RNG = RNG.Simple(System.currentTimeMillis)): Unit = p.run(maxSize, testCases, rng) match {
    case Falsified(msg, n) =>
      println(s"! Falsified after $n passed tests:\n $msg")
    case Passed =>
      println(s"+ OK, passed $testCases tests.")
    case Proved =>
      println(s"+ OK, proved property.")
  }
}



object Gen {
  def unit[A](a: => A): Gen[A] =
    Gen(State.unit(a))
  def boolean: Gen[Boolean] =
    Gen(choose(0, 2).sample.map(_ == 1))
  def choose(start: Int, stopExclusive: Int): Gen[Int] =
    Gen(State.nonNegativeLessThan(stopExclusive - start).map(_ + start))
  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] =
    Gen(State.sequence(List.fill(n)(g.sample)))
  def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] =
    boolean.flatMap(if (_) g1 else g2)
  def weighted[A](g1: (Gen[A],Double), g2: (Gen[A],Double)): Gen[A] = {
    val (gen1, w1) = g1
    val (gen2, w2) = g2
    Gen(State(RNG.double)).flatMap(x => if (x < w1.abs / (w1.abs + w2.abs)) gen1 else gen2)
  }
  def listOf[A](g: Gen[A]): SGen[List[A]] =
    SGen(listOfN(_, g))

  def listOf1[A](g: Gen[A]): SGen[List[A]] =
    SGen(n => listOfN(n max 1, g))

  trait Cogen[-A] {
    def sample(a: A, rng: RNG): RNG
  }

  def fn[A,B](in: Cogen[A])(out: Gen[B]): Gen[A => B] = Gen {
    State {
      rng => {
        val f = (a: A) => {
          val rng2 = in.sample(a, rng)
          out.sample.run(rng2)._1
        }
        (f, rng)
      }
    }
  }
}

case class Gen[+A](sample: State[RNG,A]) {
  def map[B](f: A => B): Gen[B] =
    Gen(sample.map(f))
  def flatMap[B](f: A => Gen[B]): Gen[B] =
    Gen(sample.flatMap(f(_).sample))
  def listOfN(size: Gen[Int]): Gen[List[A]] =
    size.flatMap(Gen.listOfN(_, this))
  def unsized: SGen[A] =
    SGen(_ => this)
  def map2[B, C](g: Gen[B])(f: (A, B) => C): Gen[C] = for {
    a <- this
    b <- g
  } yield f(a, b)
  def **[B](g: Gen[B]): Gen[(A,B)] =
    (this map2 g)((_,_))
}

case class SGen[+A](forSize: Int => Gen[A]) {
  def map[B](f: A => B): SGen[B] =
    SGen(forSize(_) map f)
  def flatMap[B](f: A => SGen[B]): SGen[B] =
    SGen(n => forSize(n) flatMap(f(_) forSize n))
  def apply(n: Int): Gen[A] = forSize(n)
}

object ** {
  def unapply[A,B](p: (A,B)) = Some(p)
}


