import week4.{Expr, Number, Sum}


def show(e: Expr): String = e match {
    case Number(n) => n.toString
    case Sum(e1, e2) => show(e1) + "+" + show(e2)

}

  show(Number(1))
  show(Sum(Number(1), Number(2)))


def isort(l: List[Int]):List[Int] = l match {
  case Nil => List()
  case y :: y1 => insert(y, isort(y1))
}

def insert(x: Int, xs: List[Int]): List[Int] = xs match {
  case List() => List(x)
  case y :: ys => if(x > y) y :: insert(x, ys) else x :: xs
}


isort(List(22,3,7,1,9))
