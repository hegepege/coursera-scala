pack(List("a", "a", "a", "b", "c", "c", "a"))

//List(List("a", "a", "a"), List("b"), List("c", "c"), List("a"))



def pack[T](xs: List[T]): List[List[T]] = xs match {
  case Nil => Nil
  case x :: xs1 => //List(x :: xs1.takeWhile( xn => xn == x )) ::: pack(xs1.dropWhile(xn => xn == x))
    val (first, rest) = xs span (y => y == x)
    first :: pack(rest)
}


encode(List("a", "a", "a", "b", "c", "c", "a"))

def encode[T](xs: List[T]): List[(T, Int)] =  {
  pack(xs) map (x  => (x.head, x.length))
}

lengthFun(List(1,2,3,4, 5, 10))


def mapFun[T, U](xs: List[T], f: T => U): List[U] =
  (xs foldRight List[U]())( (x, ys) => f(x) :: ys )

def lengthFun[T](xs: List[T]): Int =
  (xs foldRight 0)( (xs, l)  => l + 1 )


mapFun(List(1,2,3,4), (x: Int) => x + x)

mapFun(List(1,2,3,4), (x: Int) => x * x)


