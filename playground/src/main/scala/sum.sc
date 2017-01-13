def sum(f: Int => Int, a: Int, b: Int): Int = {

  def loop(a: Int, acc: Int): Int = {
    if (a > b) acc
    else loop(a + 1, acc + f(a))
  }

  loop(a, 0)
}


sum(x => x, 0, 10)



def product(f: Int => Int)(a: Int, b: Int): Int =
  if (a > b) 1
  else f(a) * product(f)(a + 1, b)

def factorial(n: Int): Int =
  product(x => x)(1, n)


product(x => x)(1, 6)
product(x => x * x)(3,7)


factorial(6)


def product2(f: Int => Int)(a: Int, b: Int): Int = mapReduce(f, (x,y) => x *y , 1) (a, b)

product2(x => x)(1, 6)
product2(x => x * x)(3,7)

def mapReduce(f: Int => Int, combine: (Int, Int) => Int, default: Int)(a: Int, b: Int): Int =
  if(a > b) default
  else combine(f(a), mapReduce(f, combine, default)(a+1, b))
