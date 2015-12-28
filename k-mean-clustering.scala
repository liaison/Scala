
import scala.io.Source
import scala.collection.immutable.List

/**
 *  Implement the K-mean-clustering algorithm.
 * 
 */
object K_Mean_Clustering {

    def process(input: List[String], k: Int) {
        val points = parse(input)

        println("points:")
        points.foreach(print)
        println()

        val centroids = initCentroids(points, k)

        println("centroids:")
        centroids.foreach(println)

        val clusters = cluster(points, centroids)
        printClusters(clusters)
    }

    /**
     *  not need to be precise (doing the square root)
     */
    def distance(a: (Int, Int), b: (Int, Int)) : Int = {
        val dx = a._1 - b._1
        val dy = a._2 - b._2
        dx * dx + dy * dy
    }

    def printClusters(clusters: Array[Set[(Int, Int)]]) {
        println("clusters:")
        clusters.foreach{ set =>
            set.foreach(print)
            println()
        }
    }

    /**
     *  cluster the points, given the centroids.
     */
    def cluster(points: Array[(Int, Int)], centroids: Array[(Int, Int)]) = {
        val clusters = new Array[Set[(Int, Int)]](centroids.length)
        // initialize sets, otherwise NULL pointer exception.
        (1 to centroids.length).foreach(i=> clusters(i-1) = Set[(Int, Int)]())

        // query for the closest centroid.
        def closestCentroid(p: (Int, Int)) : Int = {
            var min_distance = Int.MaxValue
            var min_index = 0
            var index = 0
            centroids.foreach{ c =>
                val d = distance(c, p)
                if( d < min_distance ) {
                    min_index = index
                    min_distance = d
                }
                index += 1
            }
            // return the closest centroid.
            min_index
        }

        points.foreach{ p =>
            val c = closestCentroid(p)
            clusters(c) += p
        }

        clusters
    }

    /**
     * Initialize the centroid for each cluster.
     */
    def initCentroids(points: Array[(Int, Int)], k: Int) : Array[(Int, Int)] = {
        // randomly pick k unique candidates.
        val candidates = rand_k_of_n(k, points.length)

        // retrieve the candidate points.
        candidates.map{points(_)}
    }

    /**
     *  Generate a sorted array of k unique numbers out of the range n.
     *   [A1, A2... Ak]   0 <= Ai <= n-1
     *
     *   require (k <= n)
     */
    def rand_k_of_n(k: Int, n: Int) : Array[Int] = {
        val candidates = scala.collection.mutable.Set[Int]()
        val oracle = new scala.util.Random
        val ret = new Array[Int](k)

        // parameter check, otherwise, endless loop.
        if(k > n) return ret

        def unique_rand() : Int = {
            val c = oracle.nextInt(n)
            if (candidates.contains(c)) return unique_rand()
            candidates += c;
            c
        }

        (1 to k).foreach(i => ret(i-1) = unique_rand())
        ret.sortWith(_<_)
    }

    /**
     *  Parse the input file to get the coordination of points.
     */
    def parse(input: List[String]) : Array[(Int, Int)] = {
        input.map{ line => val XY = line.split(",").map(_.trim)
                    (XY(0).toInt, XY(1).toInt) }.toArray
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
