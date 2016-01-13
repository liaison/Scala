

/**
 *
 * A singleton utils object that contains common and useful functions.
 */
object Utils {

  /**
   * A template function to print the list.
   */
  def print_list[T](header: String, list: List[T]) {
    println("")
    println(header)
    list.foreach{ x => print("\t" + x) }
    println()
  }


  /**
   *  Print a matrix, a dense one (list of list)
   */
  def print_matrix[T](header: String, matrix: List[List[T]]) {
    println(header)
    matrix.foreach{ row =>
      row.foreach{ x => print("\t"+x) }
      println()
    }
  }


  /**
   * A template function to print a 'mutable' map.
   */
  def print_map[K, V](header: String, map: scala.collection.mutable.Map[K, V]) {
    println("")
    println(header)
    map.foreach{ case (key, value) => println(s"${key}: ${value}") }
  } 
}


