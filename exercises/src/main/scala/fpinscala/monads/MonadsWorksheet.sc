import fpinscala.monads._


List(1,2,3).flatMap(a => List(a, a * 10))

def map2[A,B,C](ma: List[A], mb: List[B])(f: (A, B) => C): List[C] =
  ma.flatMap(a => mb.map(b => f(a, b)))

map2(List(1,2,3), List(40,50))((_, _))

def replicateList[A](n: Int, ma: List[A]): List[List[A]] =
  if (n <= 0) List(List[A]())
  else map2(ma, replicateList(n - 1, ma))(_ :: _)

replicateList(4, List(1, 2, 3)).length

def filterM[A](ms: List[A])(f: A => List[Boolean]): List[List[A]] =
  ms.foldRight(List(List[A]()))(
    (a, acc) =>
      f(a).flatMap(b => if (b) acc.map(a :: _) else acc))

filterM(List(1, 2, 3))(a => List(true, false))

