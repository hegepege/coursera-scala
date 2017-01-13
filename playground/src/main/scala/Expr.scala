package week4

trait Expr {

  def eval: Int = this match {
    case Number(n) => n
    case Sum(e, f) => e.eval + f.eval
  }


}

case class Number(n: Int) extends Expr
case class Sum(e1: Expr, e2: Expr) extends Expr
