

import scala.io.Source

/**
 *  Find out the closest pair of points in O(nlgn).
 */
object ClosestPair {

    def parse(input: Array[String]) = {
        input.map{l =>
            val XY = l.split("\\s+")
            (XY(0).toInt, XY(1).toInt)
        }
    }

    def process(input: Array[String]) {
        val points = parse(input)
        // input points in their original order.
        println("Original Input:")
        points.foreach(println)

        val sorted_points_on_X = points.sortWith{(A, B) => A._1 < B._1}

        println("Sorted Input:")
        sorted_points_on_X.foreach(println)

        println("Closest Distance:")
        val d_pow = closest_distance(sorted_points_on_X)
        println(math.sqrt(d_pow))
    }

    /**
     *  A divide-and-conquer algorithm to calculate the closest distance
     *    between two points in a two-dimension space.
     *
     *  The complexity of the algorithm can be expressed in the formula:
     *     T(n) = 2*(T(2/n)) + O(n)
     *    which is similar with the merge-sort algorithm.
     *
     *  Based on the master theorem, we can conclude that the overall
     *   complexity of the algorithm is O(nlgn)
     *
     */
    def closest_distance(points: Array[(Int, Int)]) : Double = {
        if(points.length < 2) return Int.MaxValue.toDouble
        if(points.length == 2) return distance_power(points(0), points(1))

        // Divide and conquer
        val mid = points.length / 2
        val splits = points.splitAt(mid)
        val left_min = closest_distance(splits._1)
        val right_min = closest_distance(splits._2)

        val side_min = if(left_min < right_min) left_min else right_min

        val sorted_points_on_Y = points.sortWith{ (A, B) => A._2 < B._2 }
        val index_limit = sorted_points_on_Y.length - 1
        val cross_min = sorted_points_on_Y.foldLeft((0, side_min)){ (cxt, E) =>
            val index = cxt._1
            val min = cxt._2
            /* This is essential part of the algorithm.
             * The min distance across two parts, is bounded within
             *   the rectangle of (side_min, 2*side_min) and within
             *   this rectangle could be at most 6 points.
             * Therefore this f(n) function, which is apart from the T(n/2)
             *  is bounded with O(n), instead of O(n^2) if we use the naive
             *  cartesian product between the divided points.
             */
            val bound = if (index+6 < index_limit) (index+6) else index_limit
            var new_min = side_min
            (index+1 to bound).foreach{ i =>
                val d = distance_power(sorted_points_on_Y(index),
                                       sorted_points_on_Y(i))
                if(d<new_min) new_min = d
            }
            // move on to the next element.
            (index+1, new_min)
        }._2

        if(side_min < cross_min) side_min else cross_min
    }

    /**
     *  The power of the Euclidean distance.
     */
    def distance_power(A: (Int, Int), B: (Int, Int)) : Double = {
        math.pow(A._1-B._1, 2) + math.pow(A._2-B._2, 2)
    }

    def main(args: Array[String]) {
        
        if( args.length > 0 ) {
            val input = scala.io.Source.fromFile(args(0)).getLines.toArray
                        .filterNot{l => l.startsWith("#") || l.trim == ""}
            process(input)
        } else {
            Console.err.println("Missing arguments!")
            println("Usage: \n\t")
        }
    }
}

