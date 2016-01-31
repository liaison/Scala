
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
    def parse(input: List[String]) : List[(Double, Array[Double])] = {
        input.map{ s =>
          val yX = s.split(",").map(_.trim.toDouble)
          (yX.head, yX.tail)
        }
    }

    /**
     * Run the gradient descent iteration, until it converges.
     *  @return the weight vector of the linear regression.
     */
    def gradient_descent(theta: Array[Double],
                         points: List[(Double, Array[Double])],
                         learningRate: Double,
                         precision: Double)
      : Array[Double] = {

      var theta_old = theta
      var iteration = 0
      var converge = false

      do {
        iteration += 1

        val gradients = calculate_gradients(theta_old, points)
 
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
              s"number of iterations: ${iteration}")

      // return the new weight vector 
      theta_old
    }

    /**
     *  calculate the gradients given the weight vector and input.
     */
    def calculate_gradients(theta: Array[Double],
                            points: List[(Double, Array[Double])])
      : Array[Double] = {

      val gradients = new Array[Double](theta.size)

      points.foreach{ case (y, x_vec) =>
        val diff = h_func(theta, x_vec) - y
        gradients(0) += diff

        (0 to x_vec.size-1).foreach{ index =>
          gradients(index+1) += x_vec(index) * diff
        }
      }

      val m = points.size
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


    def process(input: List[String], learningRate: Double, precision: Double) {
      val points = parse(input)
      val theta_init = new Array[Double](points.head._2.size + 1)
      val theta_new =
        gradient_descent(theta_init, points, learningRate, precision)
    
      Utils.print_array("gradient_descent results:", theta_new, "\t")
    }


    def main(args: Array[String]) {
       
        if (args.length > 0) {
            val input = Source.fromFile(args(0)).getLines.toList
                              .filterNot{l => l.startsWith("#") || l.trim == ""}
            
            val learningRate = if(args.size >= 2) args(1).toDouble else 0.01
            val precision = if(args.size >=3) args(2).toDouble else 0.0001

            process(input, learningRate, precision)

        } else {
            Console.err.println("Missing arguments!")
            println("Usage:\n" + 
              "\tscala gradient_descent.scala input.file" +
              // optional arguments
              "[learningRate] [precision]\n\t")
        }
    }
}


