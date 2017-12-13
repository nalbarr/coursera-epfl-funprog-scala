package recfun
import common._
import scala.collection.mutable.ListBuffer

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      print("\n")
    }
  }

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int = {
    def loop(c: Int, r:Int): Int = {
      if (c == 0 || r == 0 || c == r) 
    	1
      else 
        loop(c - 1, r - 1) + loop (c, r - 1)
    }
    loop(c, r)
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    def loop(chars: List[Char], acc: Int): Boolean = {
      if (chars.isEmpty) {
        (acc == 0) 
      } else {
        if (acc < 0) false
        else loop(chars.tail, accumulate(chars, acc) )
      }
    }
    def dump(chars: List[Char], acc: Int) {   	  
      //println ("char: " + chars.head + " acc: " + acc)
    }
    def accumulate(chars: List[Char], acc: Int): Int = chars.head match {
    	case '(' => dump(chars, acc + 1); acc + 1
    	case ')' => dump(chars, acc - 1); acc - 1
    	case _ => dump(chars, acc); acc
    }
    loop(chars, 0)	// seed with zero
  }

  /**
   * Exercise 3
   */
	def countChange(money: Int, coins: List[Int]): Int = {
      def loop(matrix: List[(Int, Int)], acc: Int): Int = {
      if (matrix.isEmpty) {
        acc
      } else {
        val b = ListBuffer[(Int, Int)]()
        var acc2 = acc
        for ((lastMax, total) <- matrix) {
          if (total < money) {
            for (c <- coins) {
              if (c >= lastMax) {
                val e = (c, total + c)
                b += e
              }
            }
          } else if (total == money) {
            acc2 += 1
          }
        }
        loop(b.toList, acc2)
      }
    }
    val b = coins.map { c => (c, c) }
    loop(b, 0)
  } 
}
