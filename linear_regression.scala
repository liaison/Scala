/*
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements.  See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License.  You may obtain a copy of the License at
 *
 *    http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
import Utils._

import scala.io.Source
import scala.collection.mutable.ListBuffer

/**
 * The approach of the Least Square to obtain the linear regression model.
 *  using the inverse of matrix in linear algebra, instead of the 
 *   iterative algorithm (gradient descent)
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
            val yX = s.split(",").map(_.trim.toFloat).toList
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
     */
    def A_T_dot_A(tmatrix: List[List[Float]]) = {
        tmatrix.map{ row =>
            tmatrix.map{ col =>
               row.zip(col).foldLeft(0F){(sum,elem) => sum + elem._1 * elem._2}
            }
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

        Utils.print_list("y:", y)
        Utils.print_matrix("X:", X)

        val X_T = transpose(X)
        Utils.print_matrix("transposed X:", X_T)

        val X_T_dot_X = A_T_dot_A(X_T)
        Utils.print_matrix("X_T * X", X_T_dot_X)

        val elems = X_T_dot_X.flatMap{l=>l}.toArray
        val X_T_dot_X_inverse = inverse_matrix_of_2_by_2(elems)
        Utils.print_matrix("(X_T * X)^-1 (inversion)", X_T_dot_X_inverse)

        val X_T_dot_y = inner_product(X_T, y)
        Utils.print_list("X_T * y", X_T_dot_y)

        val beta = inner_product(X_T_dot_X_inverse, X_T_dot_y)
        Utils.print_list("beta:", beta)
    }

    /*
     * It is not an easy task to inverse any matrix.
     *  which involves LU-decomposition operations.
     *
     * This function just perform an inversion on a fixed size matrix 2*2
     *      | a b |
     *    A | c d |                     |d  -b|
     *                 A_T = 1/(a*d-b*c)|-c  a|
     */
    def inverse_matrix_of_2_by_2(elems: Array[Float]) = {
        val a = elems(0)
        val b = elems(1)
        val c = elems(2)
        val d = elems(3)
        val factor = a*d - b*c
        val A_T = new Array[List[Float]](2)
        A_T(0) = List(d/factor, (0-b)/factor)
        A_T(1) = List((0-c)/factor, a/factor)
        A_T.toList
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


