
import scala.io.Source
import scala.collection.immutable.List

/**
 *  Implement the K-mean-clustering algorithm.
 * 
 */
object K_Mean_Clustering {

    def process(input: List[String], k: Int) {
        val points = parse(input)

        println(points)
    }

    /**
     * Initialize the centroid for each cluster.
     */
    def initCentroids(points: List[(Int, Int)], k: Int) : Array[(Int, Int)] = {
    
    }

    /**
     *  Generate a sorted list of k unique numbers out of the range n.
     */
    def rand_k_of_n(k: Int, n: Int) : List[Int] = {
        val candidates = scala.collection.mutable.Set[Int]()
        val oracle = new scala.util.Random
        val ret = Array[Int](k)
        
        def unique_rand() : Int = {
            val c = oracle.nextInt(n)
            if (candidates.contains(c)) return unique_rand()
            candidates += c;
            c
        }

        (1 to k).foreach(i => ret(i) = unique_rand())
        ret.sortWith(_>_).toList
    }

    /**
     *  Parse the input file to get the coordination of points.
     */
    def parse(input: List[String]) : List[(Int, Int)] = {        
        input.map{ line => val XY = line.split(",").map(_.trim)
                    (XY(0).toInt, XY(1).toInt) }
    }

    def main(args: Array[String]) {
    
        if (args.length > 1) { 
            val input =
              Source.fromFile(args(0)).getLines.toList.filter(
                _.startsWith("#") == false)

            val k = args(1).toInt
            
            process(input, k)
            
        } else {
            Console.err.println("Error: missing arguments!")
        }
    }

}
