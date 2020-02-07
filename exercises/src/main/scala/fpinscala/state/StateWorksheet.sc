import fpinscala.state._

val rng = RNG.Simple(0).nextInt._2

RNG.nonNegativeInt(rng)
RNG.double(rng)
RNG.intDouble(rng)
RNG.doubleInt(rng)
RNG.double3(rng)
RNG.ints(5)(rng)
RNG.ints(0)(rng)
RNG.ints(-3)(rng)
RNG.double2(rng)

RNG.map2(RNG.ints(3), RNG.double2)(_.toString + ", " + _.toString)(rng)

RNG.sequence(List(RNG.nonNegativeInt _, RNG.double _))(rng)

RNG.ints2(5)(rng)
RNG.ints2(0)(rng)
RNG.ints2(-3)(rng)

RNG.sequence(List(RNG.unit(1), RNG.unit(2), RNG.unit(3)))(rng)

RNG.nonNegativeLessThan(4232237)(rng)

RNG.map2(RNG.ints(3), RNG.double2)(_.toString + ", " + _.toString)(rng)

State(RNG.ints2(3)).map2(State(RNG.double2))(_.toString + ", " + _.toString).run(rng)

State.ns.run(rng)
State.ns2.run(rng)



State.simulateMachine(List(Coin, Coin, Turn)).run(Machine(true, 5, 10))
State.simulateMachine2(List(Coin, Coin, Turn)).run(Machine(true, 5, 10))



