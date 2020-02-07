package fpinscala.state


trait RNG {
  def nextInt: (Int, RNG) // Should generate a random `Int`. We'll later define other functions in terms of `nextInt`.
}

object RNG {
  // NB - this was called SimpleRNG in the book text

  case class Simple(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL // `&` is bitwise AND. We use the current seed to generate a new seed.
      val nextRNG = Simple(newSeed) // The next state, which is an `RNG` instance created from the new seed.
      val n = (newSeed >>> 16).toInt // `>>>` is right binary shift with zero fill. The value `n` is our new pseudo-random integer.
      (n, nextRNG) // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.
    }
  }

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A,B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (a, rng2) = rng.nextInt
    if (a == Int.MinValue) nonNegativeInt(rng2) else (Math.abs(a), rng2)
  }

  def double(rng: RNG): (Double, RNG) = {
    val (a, rng2) = nonNegativeInt(rng)
    (a / (Int.MaxValue + 1.0), rng2)
  }

  val double2: Rand[Double] =
    map(nonNegativeInt)(_ / (Int.MaxValue + 1.0))

  def intDouble(rng: RNG): ((Int,Double), RNG) = {
    val (i, rng2) = rng.nextInt
    val (a, rng3) = double(rng2)
    ((i, a), rng3)
  }

  def doubleInt(rng: RNG): ((Double,Int), RNG) = {
    val ((i, a), rng2) = intDouble(rng)
    ((a, i), rng2)
  }

  def double3(rng: RNG): ((Double,Double,Double), RNG) = {
    val (a1, rng1) = double(rng)
    val (a2, rng2) = double(rng1)
    val (a3, rng3) = double(rng2)
    ((a1, a2, a3), rng3)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    lazy val (sInt, sRng) = Stream.iterate((0, rng))(_._2.nextInt).slice(1, count + 1).unzip
    (sInt.toList, sRng.lastOption.getOrElse(rng))
  }

  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    rng => {
      val (a, r1) = ra(rng)
      val (b, r2) = rb(r1)
      (f(a, b), r2)
    }

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] =
    fs.foldRight(unit(List[A]()))(map2(_, _)(_ :: _))

  def ints2(count: Int): Rand[List[Int]] =
    sequence(List.fill(count)(int))

  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] =
    rng => {
      val (a, r) = f(rng)
      g(a)(r)
    }

  def nonNegativeLessThan(n: Int): Rand[Int] =
    flatMap(nonNegativeInt) {i =>
      val mod = i % n
      if (i + (n-1) - mod >= 0) unit(mod) else nonNegativeLessThan(n)
    }

  def mapViaFlatMap[A, B](ra: Rand[A])(f: A => B): Rand[B] =
    flatMap(ra)(a => unit(f(a)))

  def map2ViaFlatMap[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    flatMap(ra)(a => mapViaFlatMap(rb)(b => f(a, b)))
}

case class State[S,+A](run: S => (A, S)) {
  def flatMap[B](f: A => State[S, B]): State[S, B] =
    State(s => {
      val (a, s1) = run(s)
      f(a) run s1
    })

  def map[B](f: A => B): State[S, B] =
    flatMap(a => State.unit(f(a)))

  def map2[B, C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
    for {
      a <- this
      b <- sb
    } yield f(a, b)

//  def map[B](f: A => B): State[S, B] =
//    State(s => {
//      val (a, s1) = run(s)
//      (f(a), s1)
//    })
}

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object State {
  def unit[S, A](a: A): State[S, A] =
    State(s => (a, s))
  def sequence[S, A](ls: List[State[S, A]]): State[S, List[A]] =
    ls.foldRight(unit[S, List[A]](Nil))(_.map2(_)(_ :: _))
  def traverse[S, A, B](ls: List[State[S, A]]): State[S, List[A]] =
    ls.foldRight(unit[S, List[A]](Nil))(_.map2(_)(_ :: _))
  type Rand[A] = State[RNG, A]

  def modify[S](f: S => S): State[S, Unit] = for {
    s <- get // Gets the current state and assigns it to `s`.
    _ <- set(f(s)) // Setsz the new state to `f` applied to `s`.
  } yield ()

  def modify2[S](f: S => S): State[S, Unit] =
    get.flatMap(set[S] _ compose f)

  def get[S]: State[S, S] = State(s => (s, s))

  def set[S](s: S): State[S, Unit] = State(_ => ((), s))

  private def nextMachine(i: Input) = (m: Machine) => (i, m) match {
    case (_, Machine(_, candies, _)) if candies <= 0 => m
    case (Coin, Machine(false, _, _)) => m
    case (Turn, Machine(true, _, _)) => m
    case (Coin, Machine(true, candies, coins)) => Machine(false, candies, coins + 1)
    case (Turn, Machine(false, candies, coins)) => Machine(true, candies - 1, coins)
  }

  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = for {
    _ <- inputs.foldLeft(unit[Machine, Unit](()))((state, input) => state.flatMap(_ => modify(nextMachine(input))))
    m <- get
  } yield (m.coins, m.candies)

  def simulateMachine2(inputs: List[Input]): State[Machine, (Int, Int)] = for {
    _ <- sequence(inputs map (modify[Machine] _ compose nextMachine))
    m <- get
  } yield (m.coins, m.candies)

  val int: Rand[Int] = State(RNG.int)

  def nonNegativeLessThan(n: Int): Rand[Int] = State(RNG.nonNegativeLessThan(n))

  def ints(count: Int): Rand[List[Int]] =
    State(RNG.ints(count))

  val ns: Rand[List[Int]] =
    nonNegativeLessThan(10).flatMap(x =>
      int.flatMap(y =>
        ints(x).map(xs =>
          xs.map(_ % y))))

  val ns2: Rand[List[Int]] = for {
    x <- nonNegativeLessThan(10)
    y <- int
    xs <- ints(x)
  } yield xs.map(_ % y)
}
