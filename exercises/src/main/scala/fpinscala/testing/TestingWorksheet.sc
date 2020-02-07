import fpinscala.parallelism.Par
import fpinscala.parallelism.Par.Par
import fpinscala.testing._
//(new Prop { def check = true} && new Prop { def check = true}).check
//(new Prop { def check = true} && new Prop { def check = false}).check

val smallInt = Gen.choose(-10,10)
val maxProp = Prop.forAll(Gen.listOf1(smallInt)) { ns =>
  val max = ns.max
  !ns.exists(_ > max)
}
Prop.run(maxProp)


val sortProp = Prop.forAll(Gen.listOf1(smallInt)) { ns =>
  val nss: List[Int] = ns.sorted
  (nss.isEmpty || nss.tail.isEmpty || !nss.zip(nss.tail).exists {
    case (a,b) => a > b
  }) &&
    !ns.exists(!nss.contains(_)) &&
    !nss.exists(!ns.contains(_))
}
Prop.run(sortProp)

val p2 = Prop.checkPar { Par.equal (
    Par.map(Par.unit(1))(_ + 1),
    Par.unit(2)
  )
}

Prop.run(p2)

val pint = Gen.choose(0,10) map (Par.unit _)

val p4 = Prop.forAllPar(pint)(n => Prop.equal(Par.map(n)(y => y), n))
Prop.run(p4)


val p5 = Prop.forAllPar(pint)(n => Prop.equal(Par.fork(n), n))
Prop.run(p5)


