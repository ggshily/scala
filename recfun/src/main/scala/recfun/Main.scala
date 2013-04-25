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
    def countParenthese(chars: List[Char], count: Int): Boolean = {
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
	
	countParenthese(chars, 0)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    if(money <= 0 || coins.isEmpty)
		return 0;
  
    coins.sorted
  
    def loop(n: Int, m: Int): Int = {
	  if(n == 0) return 1;
	  if(n < 0) return 0;
	  if(m < 0 && n >= 1) return 0;
	  
	  return loop(n, m - 1) + loop(n - coins(m), m);
	}
	
	loop(money, coins.size - 1)
  }
  
}
