{
  import fpinscala.parallelism.Par._
  import java.util.concurrent.Executors

  def res[A](nThreads:Int)(p: Par[A]) =
    println(run(Executors.newFixedThreadPool(nThreads))(p).get)

  res(2)(choiceN(unit(1))(List(unit("a"), unit("b"), unit("c"))))
  res(2)(choice(unit(false))(unit("b"), unit("c")))
  res(2)(choiceViaChoiceN(unit(false))(unit("b"), unit("c")))
  res(2)(choiceMap(unit(5))(Map(4 -> unit("40"), 5 -> unit("50"))))
  res(2)(choiceNChooser(unit(1))(List(unit("a"), unit("b"), unit("c"))))
  res(2)(choiceViaChooser(unit(false))(unit("b"), unit("c")))
  res(2)(join(unit(unit(123))))
  res(2)(joinViaFlatMap(unit(unit(123))))
}
{
  import fpinscala.parallelism.Nonblocking.Par._
  import fpinscala.parallelism.Nonblocking.Par
  import java.util.concurrent.Executors

  def res[A](nThreads:Int)(p: Par[A]) =
    println(run(Executors.newFixedThreadPool(nThreads))(p))

  res(2)(choiceN(unit(1))(List(unit("a"), unit("b"), unit("c"))))
  res(2)(choice(unit(false))(unit("b"), unit("c")))
  res(2)(choiceViaChoiceN(unit(false))(unit("b"), unit("c")))
  res(2)(choiceMap(unit(5))(Map(4 -> unit("40"), 5 -> unit("50"))))
  res(2)(choiceNChooser(unit(1))(List(unit("a"), unit("b"), unit("c"))))
  res(2)(choiceViaChooser(unit(false))(unit("b"), unit("c")))
  res(2)(join(unit(unit(123))))
  res(2)(joinViaFlatMap(unit(unit(123))))
}

