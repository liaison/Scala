
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

        var centroids = initCentroids(points, k)
        var continue = true

        while (continue) {
            println("\ncentroids:")
            centroids.foreach(println)

            val clusters = cluster(points, centroids)
            printClusters(clusters)

            val new_centroids = update_centroids(clusters)

            if ( centroids.corresponds(new_centroids){_==_} ) continue = false
            else centroids = new_centroids
        }
    }

    /**
     * Initialize the centroid for each cluster.
     */
    def initCentroids(points: Array[(Float, Float)], k: Int)
        : Array[(Float, Float)] = {

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
            val newBee = oracle.nextInt(n)
            if (candidates.contains(newBee)) return unique_rand()
            candidates += newBee;
            newBee
        }

        (1 to k).foreach(i => ret(i-1) = unique_rand())
        ret.sortWith(_<_)
    }

    /**
     *  cluster the points, given the centroids.
     */
    def cluster(points: Array[(Float, Float)], centroids: Array[(Float, Float)]) = {
        val clusters = new Array[Set[(Float, Float)]](centroids.length)
        // initialize sets, otherwise NULL pointer exception.
        (1 to centroids.length).foreach(i=> clusters(i-1) = Set[(Float, Float)]())

        // Query for the closest centroid.
        def closestCentroid(p: (Float, Float)) : Int = {
            var min_distance = Float.MaxValue
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

        // Assign the point to its closest cluster.
        points.foreach{ p =>
            val c = closestCentroid(p)
            clusters(c) += p
        }
        // Return the cluster.
        clusters
    }

    /**
     * Update/calculate the centroids based on the clusters
     */
    def update_centroids(clusters: Array[Set[(Float, Float)]]) = {
        clusters.map{ set =>
            val sum = set.foldLeft((0F,0F))((A, B) => (A._1+B._1, A._2+B._2))
            (sum._1/set.size, sum._2/set.size)
        }
    }

    /**
     *  not need to be precise (doing the square root)
     */
    def distance(a: (Float, Float), b: (Float, Float)) : Float = {
        val dx = a._1 - b._1
        val dy = a._2 - b._2
        dx * dx + dy * dy
    }

    def printClusters(clusters: Array[Set[(Float, Float)]]) {
        println("clusters:")
        clusters.foreach{ set =>
            set.foreach(print)
            println()
        }
    }

    /**
     *  Parse the input file to get the coordination of points.
     */
    def parse(input: List[String]) : Array[(Float, Float)] = {
        input.map{ line => val XY = line.split(",").map(_.trim)
                    (XY(0).toFloat, XY(1).toFloat) }.toArray
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
