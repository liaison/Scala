import scala.io.Source

/**
 * Merge sort algorithm
 *
 */
import scala.io.Source
object MergeSort {

  /**
   *  the classic quick sort algorithm
   */
  def merge_sort(list : Array[Int]) : Array[Int] = {
      // bottom case
      if(list.length < 2) return list

      // Divide and conquer
      val mid = list.length / 2
      val splits = list.splitAt(mid)
      val left = merge_sort(splits._1)
      val right = merge_sort(splits._2)

      // Merge the results in O(n)
      var l_cursor = 0
      val l_bound = left.length
      var r_cursor = 0
      val r_bound = right.length
      val sorted_list = new Array[Int](list.length)
      while( l_cursor < l_bound ||
             r_cursor < r_bound ) {
          val merge_index = l_cursor + r_cursor
          if(l_cursor == l_bound) {
              sorted_list(merge_index) = right(r_cursor)
              r_cursor += 1
          } else if(r_cursor == r_bound) {
              sorted_list(merge_index) = left(l_cursor)
              l_cursor += 1
          } else if(left(l_cursor) < right(r_cursor)) {
              sorted_list(merge_index) = left(l_cursor)
              l_cursor += 1
          } else {
              sorted_list(merge_index) = right(r_cursor)
              r_cursor += 1
          }
      }
      // return the sorted list
      sorted_list
  }


  /**
   *  Parse the input file into a list of integers
   */
  def parse(input: Array[String]) : Array[Int] = {
      // concatenate all the numbers seperated by \n and spaces.
      input.flatMap { line => line.split("\\s+").map{_.toInt} }
  }


  /**
   *  The main function !
   */
  def main(args : Array[String]) {

    if (args.length > 0) { 
      val input = Source.fromFile(args(0)).getLines.toArray
                    .filterNot(l => l.startsWith("#") || l.trim == "")

      val list = parse(input)
      println("Before merge_sort: ")
      list.foreach(println)
      println("After sorting: ")
      merge_sort(list).foreach(println)

    } else {
        Console.err.println("Error: missing the input file!")
        println("Usage:\n\t Scala MergeSort input.file")
    }

  } // end of main

} // end of the singleton object.

