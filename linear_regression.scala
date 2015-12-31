
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
     * Transpose the matrix.
     * Note: apparently, the scala List library provides the transpose() API.
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

    /*
     * The inner product of one's transposition with oneself.
     *  i.e.  A^T * A
     *        = X'  where X'_i = sum((A^T_j)^2)
     */
    def A_T_dot_A(tmatrix: List[List[Float]]) = {
        tmatrix.map{ row =>
            row.map{x => x*x}.foldLeft(0F)(_+_)
        }
    }

    /*
     * The inner product of a matrix and a vector.
     *
     */
    def inner_product(matrix: List[List[Float]], vector: List[Float])
        : List[Float] = {
        matrix.map{ row =>
            row.zip(vector).foldLeft(0F){ (sum, elem) =>
                sum + (elem._1 * elem._2)
            }
        }
    }

    def process(input: List[String]) {
        println(input)

        val yX = parse(input)
        val y = yX._1
        val X = yX._2

        println("y:")
        println(y)

        println("X:")
        printMatrix(X)

        println("transposed X")
        val X_T = transpose(X)
        printMatrix(X_T)

        val X_T_dot_X = A_T_dot_A(X_T)
        val X_T_dot_y = inner_product(X_T, y)
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


