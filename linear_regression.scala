
import scala.io.Source
import scala.collection.mutable.ListBuffer

/**
 * The approach of the Least Square to obtain the linear regression model.
 * https://en.wikipedia.org/wiki/Linear_least_squares_(mathematics)
 *
 */
object LinearRegression {

    /*
     * Parse the input into a tuple of (y, X)
     *  where y is list of values for the dependent variable,
     *  and X is a list of vector of the values of the independent variable.
     */
    def parse(input: List[String]) : (List[Float], List[List[Float]]) = {
        input.map{ s =>
            val yX = s.split("\\s+").toList.map(_.toFloat)
            (yX.head, yX.tail)
        }.unzip
    }

    /*
     * Transpose the matrix
     */
    def transpose(matrix: List[List[Float]]) : List[List[Float]] = {
        // Initialize the transposed matrix.
        val tmatrix = new Array[ListBuffer[Float]](matrix.head.length)
                          .map{l => new ListBuffer[Float]()}

        matrix.foreach{ row =>
            val column_index = 0
            row.foldLeft(column_index){ (index, x) =>
                tmatrix(index) += x
                index+1
            }
        }
        tmatrix.toList.map(_.toList)
    }

    def process(input: List[String]) {
        println(input)

        val yX = parse(input)
        println(yX._1)
        printMatrix(yX._2)

        println("transposed X")
        printMatrix(transpose(yX._2))
    }

    def printMatrix(matrix: List[List[Float]]) {
        matrix.foreach{ row =>
            row.foreach{ x => print("\t"+x) }
            println()
        }
    }

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
}


