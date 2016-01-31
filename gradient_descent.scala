
import Utils._

import scala.io.Source
import scala.collection.mutable.ListBuffer

/**
 * Using the stochastic/batch gradient descent to solve the linear regression.
 *  
 * The target function is y = m * x + b
 *   where m is the slope and b is the y-intercept
 *
 * reference: https://class.coursera.org/ml-003/lecture
 */
object GradientDescent {

    /*
     * Parse the input into a tuple of (y, x)
     *  where y is list of values for the dependent variable,
     *  and X is a list of values of the independent variable x.
     */
    def parse(input: List[String]) : List[(Double, Double)] = {
        input.map{ s =>
            val yx = s.split(",").map(_.trim.toDouble)
            (yx(0), yx(1))
        }
    }

    /**
     * Run the gradient descent iteration, until it converges.
     */
    def gradient_descent(m: Double,
                         b: Double,
                         points: List[(Double, Double)],
                         learningRate: Double,
                         precision: Double)
      : (Double, Double) = {

      var (m_new, b_new) = (m, b)
      var (m_old, b_old) = (m, b)
      var iteration = 0

      do {
        iteration += 1
        m_old = m_new
        b_old = b_new

        val (m_gradient, b_gradient) = gradients(m_old, b_old, points)
        m_new = m_old - learningRate * m_gradient
        b_new = b_old - learningRate * b_gradient

      } while ( math.abs(m_new - m_old) > precision &&
                math.abs(b_new - b_old) > precision )

      println(s"learningRate:${learningRate}\t",
              s"precision:${precision}\t",
              s"number of iterations: ${iteration}")
      (m_new, b_new)
    }

    /**
     *  the gradients for the m (slope) and b (intercept) respectively.
     */
    def gradients(m: Double,
                  b: Double,
                  points: List[(Double, Double)])
      : (Double, Double) = {
      var m_gradient = 0.0
      var b_gradient = 0.0
      points.foreach{ case (y, x) =>
        val diff = m * x + b - y
        m_gradient += diff * x
        b_gradient += diff
      }

      (m_gradient, b_gradient)
    }


    def process(input: List[String], learningRate: Double, precision: Double) {
      val points = parse(input)
      val m_init = 0
      val b_init = 0
      // val learningRate = 0.01
      // val precision = 0.00001
      val (m, b) = gradient_descent(m_init, b_init, points,
                                    learningRate, precision)
    
      println(s"gradient_descent results: m=${m}, b=${b}")
    }


    def main(args: Array[String]) {
       
        if (args.length > 2) {
            val input = Source.fromFile(args(0)).getLines.toList
                              .filterNot{l => l.startsWith("#") || l.trim == ""}
            
            val learningRate = args(1).toDouble
            val precision = args(2).toDouble

            process(input, learningRate, precision)

        } else {
            Console.err.println("Missing arguments!")
            println("Usage: scala gradient_descent.scala input.file learningRate precision\n\t")
        }
    }
}


