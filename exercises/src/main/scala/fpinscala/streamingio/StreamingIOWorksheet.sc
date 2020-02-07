import fpinscala.streamingio.SimpleStreamTransducers._

Process.take(3)(Stream(1,2,3,4,5,6,7)).toList
Process.take(9)(Stream(1,2,3,4,5,6,7)).toList

Process.drop(3)(Stream(1,2,3,4,5,6,7)).toList
Process.drop(9)(Stream(1,2,3,4,5,6,7)).toList

Process.takeWhile[Int](_ < 6)(Stream(1,2,3,4,5,6,7)).toList
Process.takeWhile[Int](_ == 10)(Stream(1,2,3,4,5,6,7)).toList
Process.takeWhile[Int](_ < 3)(Stream(1,2,3,1,2,3,4)).toList

Process.dropWhile[Int](_ < 6)(Stream(1,2,3,4,5,6,7)).toList
Process.dropWhile[Int](_ < 10)(Stream(1,2,3,4,5,6,7)).toList
Process.dropWhile[Int](_ == 1)(Stream(1,2,1,4,1,6,1)).toList

Process.count(Stream("a", "b", "c")).toList
Process.mean(Stream(1,2,3,4,5,6,7)).toList

Process.sum2(Stream(1,2,3,4,5,6,7)).toList
Process.count3(Stream("a", "b", "c")).toList



(Process.filter((_:Int) % 2 == 0) |> Process.lift(_ + 1))(Stream(1,2,3,4,5,6,7)).toList


Process.take(3).zipWithIndex(Stream("a", "b", "c", "d")).toList



