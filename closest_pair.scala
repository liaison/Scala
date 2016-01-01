

import scala.io.Source

/**
 *  Find out the closest pair of points in O(nlgn).
 */
object ClosestPair {

    def process(input: List[String]) {
        println(input)
    }

    def main(args: Array[String]) {
        
        if( args.length > 0 ) {
            val input = scala.io.Source.fromFile(args(0)).getLines.toList
                        .filterNot{l => l.startsWith("#") || l.trim == ""}
            process(input)
        } else {
            Console.err.println("Missing arguments!")
            println("Usage: \n\t")
        }
    }
}

