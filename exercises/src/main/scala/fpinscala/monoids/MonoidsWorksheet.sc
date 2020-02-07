import fpinscala.testing._
import fpinscala.monoids.Monoid._


Prop.run(monoidLaws(booleanOr, Gen.boolean))
Prop.run(monoidLaws(booleanAnd, Gen.boolean))
Prop.run(monoidLaws(intAddition, Gen.choose(-10, 10)))
Prop.run(monoidLaws(intMultiplication, Gen.choose(-10, 10)))

val optionGen = (Gen.boolean ** Gen.choose(-10, 10)).map {
  case b ** i => if (b) Some(i) else None
}

Prop.run(monoidLaws(optionMonoid[Int], optionGen))

Prop.run(monoidLaws(stringMonoid, Gen.choose(-10, 10).map(_.hashCode.toString)))


ordered(Vector(1, 2, 3))
ordered(Vector(3))
ordered(Vector(1, 1, 1, 1))
ordered(Vector())
ordered(Vector(1, 1, 2, 2, 3, 3))
ordered(Vector(2, 1))


count("lorem ipsum dolor sit amet, ")
count(" a b ")
count("   ")
count("")



