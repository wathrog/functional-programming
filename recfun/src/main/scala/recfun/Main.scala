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
    if (c < 0) throw new NoSuchElementException
    if (r < 0) throw new NoSuchElementException
    if (c > r) throw new NoSuchElementException
    pascalRec(c, r)
  }
  
  def pascalRec(c: Int, r: Int): Int = {
    c match {
      case 0 => 1
      case `r` => 1
      case _ => pascal(c-1,r-1) + pascal(c,r-1)
    }
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    balanceInt(chars, 0) == 0
  }

  def balanceInt(chars: List[Char], counter: Int): Int = {
    if (chars.isEmpty)
      counter
    else {
      chars.head match {
        case '(' => balanceInt(chars.tail, counter + 1)
        case ')' if counter > 0 => balanceInt(chars.tail, counter - 1)
        case ')' if counter == 0 => -1
        case _ => balanceInt(chars.tail, counter)
      }
    }
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    money match {
      case x if money < 0 => throw new NoSuchElementException
      case x if money == 0 => 0
      case _ => countChangeInt(money, coins.sortWith(_>_), 0)
    }
  }
  
  def countChangeInt(money: Int, coins: List[Int], solutions: Int): Int = {
    if (coins.isEmpty) solutions
    else {
      val remaining = money - coins.head
      remaining match {
        case x if remaining > 0 => countChangeInt(x, coins, solutions) + countChangeInt(x, coins.tail, solutions)
        case x if remaining < 0 => countChangeInt(money, coins.tail, solutions)
        case 0 => countChangeInt(money, coins.tail, solutions + 1)
      }
    }
  }
}
