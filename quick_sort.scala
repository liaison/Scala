
/**
 *  Some code examples to show the basic syntax and APIs of Scala.
 *
 */
import scala.io.Source
import scala.collection.immutable.List


/**
 * The singleton object that serves as the entrance of the program.
 */
object QuickSort {

  /**
   *  Print each line with its line number.
   */
  def printLine(count : Int, line : String) : Int = {
    print(count + "\t"); println(line)
    // Return the next line number.
    count + 1
  }

  /**
   *  the classic quick sort algorithm
   */
  def quick_sort(xs : List[Int]) : List[Int] = {
    if (xs.length < 2) {
      return xs
    } else {
      val pivot = xs.head
      return quick_sort(xs.filter(_<pivot)):::(quick_sort(xs.filter(_>=pivot)))
    }
  }

  /**
   *  a more functional-style implementation of quick sort.
   */
  def quick_sort_func(xs : List[Int]) : List[Int] = {
    xs match {
      case Nil => xs
      case pivot::tail =>
        val lt = tail.filter(_<pivot)
        val ge = tail.filter(_>=pivot)
        quick_sort_func(lt):::(pivot::quick_sort_func(ge))
    }
  }

  /**
   *  Parse the input file into a list of integers
   */
  def parse(input: List[String]) : List[Int] = {
      // concatenate all the numbers seperated by \n and spaces.
      input.flatMap { line => line.split("\\s+").map{_.toInt} }
  }


  /**
   *  The main function !
   */
  def main(args : Array[String]) {

    if (args.length > 0) { 
      val input = Source.fromFile(args(0)).getLines.toList
                    .filterNot(l => l.startsWith("#") || l.trim == "")

      println("input: ")
      input.foldLeft(0)(printLine)

      val list = parse(input)
      println("Before quick_sort: " + list)
      println("After sorting: " + quick_sort_func(list))

    } else {
        Console.err.println("Error: missing the input file!")
        println("Usage:\n\t Scala QuickSort input.file")
    }

  } // end of main

} // end of the singleton object.


