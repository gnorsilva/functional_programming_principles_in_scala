package recfun

import common._

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
    def aboveLeft: Int = pascal(c - 1, r - 1)

    def aboveRight: Int = pascal(c, r - 1)

    def isTriangleEdge: Boolean = c == 0 || c == r

    if (isTriangleEdge) 1 else (aboveLeft + aboveRight)
  }


  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {

    def balanceFinder(chars: List[Char], balance: Int): Boolean = {
      if (balance < 0) false
      else if (chars.isEmpty) balance == 0
      else balanceFinder(chars.tail, balance + delta(chars.head))
    }

    def delta(char: Char): Int = {
      if (char == '(') 1 else if (char == ')') -1 else 0
    }

    balanceFinder(chars, 0)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
      if (coins.isEmpty || money <= 0) 0
      else {
        val subtracted: Int = money - coins.head
        val result = if (subtracted == 0) 1 else countChange(subtracted, coins)
        result + countChange(money, coins.tail)
    }
  }
}
