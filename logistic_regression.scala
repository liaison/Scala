
import Utils._

import scala.io.Source
import scala.collection.mutable.ListBuffer

/**
 *
 */
object LogisticRegression {

    def main(args: Array[String]) {
       
        if (args.length > 0) {
            val input = Source.fromFile(args(0)).getLines.toList
                              .filterNot{l => l.startsWith("#") || l.trim == ""}
            process(input)
        } else {
            Console.err.println("Missing arguments!")
            println("Usage:\n\t")
        }
    }

    def process(input: List[String]) {
    }

    /*
     * Parse the input into a tuple of (y, X)
     *  where y is list of values for the dependent variable,
     *  and X is a list of vector of the values of the independent variable.
     */
    def parse(input: List[String]) : (List[Float], List[List[Float]]) = {
        input.map{ s =>
            val yX = s.split(",").map(_.trim.toFloat).toList
            (yX.head, yX.tail)
        }.unzip
    }


}


