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
    if(r == 0)
	  return 1
    if(c == 0)
	  return 1
	if(c == r)
	  return 1
    pascal(c - 1, r - 1) + pascal(c, r - 1)
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    def countParenthese(count: Int): Boolean = {
	  if(chars.isEmpty)
	    return count == 0;
	  if(count < 0)
	    return false
	  if (chars.head.equals('('))
	    return countParenthese(chars.tail, count + 1)
	  if (chars.head.equals(')'))
	    return countParenthese(chars.tail, count - 1)
		
	  countParenthese(chars.tail, count)
	}
	
	countParenthese(=0)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    coins.sorted
  
    def count(money: Int, coins: List[Int], counts: List[Int], sum: Int): Int = {
	  val total: Int = 0
	  
	  do {
	  
	  
	  
	  } while(success)
	  
	  return total
	}
	
	val counts = coins.map(x => 0)
	count(money, coins, counts, 0)
  }
  
}
