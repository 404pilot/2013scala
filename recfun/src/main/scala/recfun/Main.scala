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
  // row = 0..... n. col= 0...n
  def pascal(c: Int, r: Int): Int = if (c == 0 || c == r) 1 else pascal(c - 1, r - 1) + pascal(c, r - 1)

  /**
   * Exercise 2
   */
  // ??? means 'not implemented'
  def balance(chars: List[Char]): Boolean = {

    def balance(count: Int, chars: List[Char]): Boolean = {
      if (chars.isEmpty) count == 0
      else if (chars.head == ')') (count - 1) >= 0 && balance(count - 1, chars.tail)
      else if (chars.head == '(') balance(count + 1, chars.tail)
      else balance(count, chars.tail)
    }

    balance(0, chars)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    /*if (money == 0) 1
    else if (money < 0 || coins.isEmpty) 0
    else {
      var count = 0
      // be careful, start from 0 (don't consider this coin)
      for (i <- 0 to money / coins.head) count = count + countChange(money - coins.head * i, coins.tail)
      count
    }*/

    if (coins.isEmpty || money < 0) 0
    else if (money == 0) 1
    else countChange(money - coins.head, coins) + countChange(money, coins.tail)
  }
}
