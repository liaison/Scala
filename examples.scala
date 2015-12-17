
/**
 *  Some code examples to show the basic syntax and APIs of Scala.
 *
 */

import scala.io.Source
import scala.collection.immutable.List


/**
 * The singleton object that serves as the entrance of the program.
 */
object Examples {

  def widthOfLine(s : String) : Int = {
    s.length
  }

  def printLine(line : String) : Unit = {
  
    print(widthOfLine(line) + " ")
    println(line)    
  }
 
  def quick_sort(xs : List[Int]) : List[Int] = {
    if (xs.length < 2) {
      return xs
    } else {
      val pivot = xs.head
      return quick_sort(xs.filter(_<pivot)):::(quick_sort(xs.filter(_>=pivot)))
    }
  }

  /**
   *  The main function !
   */
  def main(args : Array[String]) {
    
    val list = (10 to 1 by -1).toList

    
    if (args.length > 0) {
 
      val input = Source.fromFile(args(0)).getLines.toList
      
      input.foreach(printLine)

    } else {
        Console.err.println("Error: missing the input file!")
    }
    
    
    println("Before quick_sort: " + list)
    
    println("After sorting:" + quick_sort(list))
  }
}


