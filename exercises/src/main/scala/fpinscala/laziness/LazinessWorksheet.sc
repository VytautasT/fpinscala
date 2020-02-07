import fpinscala.laziness._

Stream(1, 2, 3).toList()
Stream.empty[Int].toList()

Stream(1, 2, 3).toList2()
Stream.empty[Int].toList2()

Stream(1, 2, 3).toList3()
Stream.empty[Int].toList3()

Stream(1, 2, 3).take(2).toList3()
Stream(1, 2, 3).take(4).toList3()
Stream.empty[Int].take(2)

Stream(1, 2, 3).drop(2).toList3()
Stream(1, 2, 3).drop(4)
Stream.empty[Int].drop(2)

Stream(1, 2, 3).takeWhile(_ < 10).toList3()
Stream(1, 2, 30).takeWhile(_ < 10).toList3()
Stream.empty[Int].takeWhile(_ < 10)

Stream(1, 2, 3).forAll(x => {println(x); x < 10})
Stream(1, 20, 3).forAll(x => {println(x); x < 10})
Stream.empty[Int].forAll(x => {println(x); x < 10})

Stream(1, 2, 3).takeWhile2(_ < 10).toList3()
Stream(1, 2, 30).takeWhile2(_ < 10).toList3()
Stream.empty[Int].takeWhile2(_ < 10)

Stream(1, 2, 3).headOption
Stream.empty[Int].headOption

Stream(1, 2, 3).map(_ * 2).toList3()
Stream.empty[Int].map(_ * 2)

Stream(1, 2, 3).filter(_ < 10).toList3()
Stream(1, 2, 30).filter(_ < 10).toList3()
Stream.empty[Int].filter(_ < 10)

Stream(1, 2, 3).append(Stream(4, 5, 6)).toList3()
Stream(1, 2, 30).append(Stream(4, 5, 6)).toList3()
Stream.empty[Int].append(Stream(4, 5, 6)).toList3()

Stream(1, 20, 3).flatMap(x =>
  if (x < 10) Stream(x, 69)
  else Stream.empty[Int])
  .toList3()

Stream.constant(3).take(5).toList3()

Stream.from(11).take(3).toList3()

Stream.fibs.take(5).toList3()

Stream.unfold(0)(s => if (s != 13) Some(s % 7, s + 1) else None).take(20).toList3()

Stream.fibs2.take(5).toList3()

Stream.from2(11).take(3).toList3()

Stream.constant2(3).take(5).toList3()

Stream(1, 2, 3).map2(_ * 2).toList3()
Stream.empty[Int].map2(_ * 2)

Stream(1, 2, 3).take2(2).toList3()
Stream(1, 2, 3).take2(4).toList3()
Stream.empty[Int].take2(2)

Stream(1, 2, 3).takeWhile3(_ < 10).toList3()
Stream(1, 2, 30).takeWhile3(_ < 10).toList3()
Stream.empty[Int].takeWhile3(_ < 10)

Stream(1, 2, 3).zipWith(Stream("a", "b", "c", "d"))(_.toString + _).toList3()

Stream(1, 2, 3).zipAll(Stream("a", "b", "c", "d")).toList3()

Stream(1, 2, 3).startsWith(Stream())
Stream(1, 2, 3).startsWith(Stream(1, 2))
Stream(1, 2, 3).startsWith(Stream(1, 2, 3))
Stream().startsWith(Stream())
Stream().startsWith(Stream(2, 3))
Stream(1, 2, 3).startsWith(Stream(2, 3))
Stream(1, 2, 3).startsWith(Stream(1, 2, 3, 4))

Stream(1, 2, 3).tails.toList3().map(_.toList3())


Stream(1, 2, 3).hasSubsequence(Stream())
Stream(1, 2, 3).hasSubsequence(Stream(1, 2))
Stream(1, 2, 3).hasSubsequence(Stream(1, 2, 3))
Stream().hasSubsequence(Stream())
Stream().hasSubsequence(Stream(2, 3))
Stream(1, 2, 3).hasSubsequence(Stream(2, 3))
Stream(1, 2, 3).hasSubsequence(Stream(1, 2, 3, 4))

Stream(1,2,3).scanRight(0)(_ + _).toList3()

