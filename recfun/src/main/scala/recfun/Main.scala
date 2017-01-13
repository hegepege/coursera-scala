package recfun

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
  }

  /**
    * Exercise 1
    */
  def pascal(c: Int, r: Int): Int = {
    if (c == 0) 1
    else if (c == r) 1
    else pascal(c - 1, r - 1) + pascal(c, r - 1)
  }


  /**
    * Exercise 2
    */
  def balance(chars: List[Char]): Boolean = {

    def balanceIter(c: Int, remainingChars: List[Char]): Boolean = {
      if (c < 0) false
      else if (remainingChars.isEmpty) c == 0
      else {
        balanceIter(handleChar(c, remainingChars.head), remainingChars.tail)
      }
    }

    def handleChar(x: Int, c: Char): Int = {
      if (c == '(') x + 1
      else if (c == ')') x - 1
      else x
    }

    balanceIter(0, chars)
  }

  /**
    * Exercise 3
    */
  def countChange(money: Int, coins: List[Int]): Int = {

    def countChangeIter(amount: Int, i: Int): Int = {
      if (amount < 0) 0
      else if (amount == 0) 1
      else if (i == coins.length && amount > 0) 0
      else countChangeIter(amount - coins(i), i) + countChangeIter(amount, i + 1)
    }

    countChangeIter(money, 0)
  }
}
