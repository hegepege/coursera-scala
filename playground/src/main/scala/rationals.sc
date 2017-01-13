
val x = new Rational(1, 3)
val y = new Rational(5, 7)
val z = new Rational(3, 2)
x.numer
x.denom
x.sub(y).sub(z)
y + y
x < y
val h = new Rational(2)

class Rational(x: Int, y: Int) {
  require(y != 0, "Denominator cannot be zero")

  private def gcd(a: Int, b: Int): Int = if (b == 0) a else gcd(b, a % b)

  private val g = gcd(x, y)

  def numer = x / g

  def denom = y / g

  //Constructor that calls the default primary constructor
  def this(x : Int) = this(x, 1)

  def < (that: Rational) = numer * that.denom < that.numer * denom

  def max(that: Rational) = if (this < that) that else this

  def + (that: Rational) =
    new Rational(numer * that.denom + that.numer * denom, denom * that.denom)

  def neg: Rational = new Rational(-numer, denom)


  def sub(that: Rational) = this + that.neg

  override def toString = if(denom == 1) numer.toString else numer + " / " + denom

}


