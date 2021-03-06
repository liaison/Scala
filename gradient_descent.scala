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
 * Using the stochastic/batch gradient descent to solve the linear regression.
 *  
 * The target function is y = t_0 * x_0 + t_1 * x_1 ... t_n * x_n
 *   where t_i is the weight, and x_0 = 1
 *
 * reference: https://class.coursera.org/ml-003/lecture
 */
object GradientDescent {

    /*
     * Parse the input into a list of tuple (y, X)
     *  where y is the dependent variable,
     *  and X is the vector of the independent variables.
     */
    def parse(input: List[String]) : Array[(Double, Array[Double])] = {
        input.map{ s =>
          val yX = s.split(",").map(_.trim.toDouble)
          (yX.head, yX.tail)
        }.toArray
    }

    /**
     * Run the gradient descent iteration, until it converges.
     *  @return the weight vector of the linear regression.
     */
    def gradient_descent(theta: Array[Double],
                         points: Array[(Double, Array[Double])],
                         learningRate: Double,
                         precision: Double,
                         stochastic: Boolean = false)
      : Array[Double] = {

      var theta_old = theta
      var iteration = 0
      var converge = false

      do {
        iteration += 1

        // It seems that in order to obtain the correct result as the gradient
        //  descent, we need to have around the same number of iterations.
        // But for each iteration, we only need to calculate one sample, instead
        //  of all input which is way much more efficient.
        // And as the side product, we achieve better precision afterwards.
        // Actually, we need to set up a higher precision for stochastic version
        // algorithm, otherwise it would converge to a 'not-so-accurate' result.
        val gradients = calculate_gradients(theta_old, points, stochastic)
 
        val theta_new =
          theta_old.zip(gradients).map{ case (theta, gradient) =>
            theta - learningRate * gradient
          }

        converge = theta_old.zip(theta_new).forall{ case (t1, t2) =>
          math.abs(t1 - t2) < precision
        }

        theta_old = theta_new

      } while (! converge)

      println(s"learningRate:${learningRate}\t" +
              s"precision:${precision}\t" +
              s"stochastic:${stochastic}\t" +
              s"No.iterations: ${iteration}")

      // return the new weight vector 
      theta_old
    }

    /**
     *  calculate the gradients given the weight vector and input.
     */
    def calculate_gradients(theta: Array[Double],
                            points: Array[(Double, Array[Double])],
                            stochastic: Boolean = false)
      : Array[Double] = {

      val gradients = new Array[Double](theta.size)

      val samples = if(stochastic) {
        val randomPick = (new scala.util.Random).nextInt(points.size)
        Array(points(randomPick))
      } else {
        points
      }

      samples.foreach{ case (y, x_vec) =>
        val diff = h_func(theta, x_vec) - y
        gradients(0) += diff

        (0 to x_vec.size-1).foreach{ index =>
          gradients(index+1) += x_vec(index) * diff
        }
      }

      val m = samples.size
      // average out the accumulated gradient
      gradients.map{ case gradient => gradient / m }
    }

    /**
     * The value of the hypothesis function, given the current weight vector.
     */
    def h_func(theta: Array[Double], X: Array[Double]): Double = {
      var sum = theta(0)
      (0 to X.size-1).foreach{ case index =>
        sum += X(index) * theta(index+1)
      }
      sum
    }


    def process(input: List[String],
                learningRate: Double,
                precision: Double,
                stochastic: Boolean = false) {
      val points = parse(input)
      val theta_init = new Array[Double](points.head._2.size + 1)
      val theta_new =
        gradient_descent(theta_init,
                         points,
                         learningRate,
                         precision,
                         stochastic)

      Utils.print_array("gradient_descent results:", theta_new, "\t")
    }


    def main(args: Array[String]) {
       
        if (args.length > 0) {
            val input = Source.fromFile(args(0)).getLines.toList
                              .filterNot{l => l.startsWith("#") || l.trim == ""}
            
            val learningRate = if(args.size >= 2) args(1).toDouble else 0.01
            val precision = if(args.size >=3) args(2).toDouble else 0.0001
            val stochastic = if(args.size >=4) args(3).toBoolean else false

            process(input, learningRate, precision, stochastic)

        } else {
            Console.err.println("Missing arguments!")
            println("Usage:\n" + 
              "\tscala gradient_descent.scala input.file" +
              // optional arguments
              "[learningRate] [precision]\n\t")
        }
    }
}


