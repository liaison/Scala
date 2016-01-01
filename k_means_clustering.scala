
import scala.io.Source
import scala.collection.immutable.List

/**
 *  Implement the K-mean-clustering algorithm and also K-means ++ which improves
 *    on the initial selection of centroids to make the algorithm in expection
 *    more optimal and also make it converge faster.
 *
 *  Usage:
 *    scala K_Mean_Clustering input.file number_of_clusters
 *
 *  The input.file contains a list of coordination in two-dimention.
 *   e.g
 *          1.0, 3.0
 *          6.2, 7.1
 *
 *   Note: the empty or comment lines (start with '#') are ignored.
 *
 */
object KMeansClustering {

    def process(input: List[String], k: Int) {
        val points = parse(input)

        println("points:")
        points.foreach(print)
        println()

        //var centroids = initCentroids(points, k)
        var centroids = k_means_plus_plus(points, k)

        var continue = true

        // Continue to do clustering, until it converges.
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
     * https://en.wikipedia.org/wiki/K-means%2B%2B
     *
     *  The vanilla version of K-means has the shortcoming that
     *    - the result is arbitrarily worse than optimum
     *    - which depends on the initialization of centroids.
     *
     *  To overcome the defect, K-means ++ algorithm has been proposed to
     *   improve the selection of initial centroids, to better stretch out
     *   the centroids at the beginning.
     *
     * In simple terms, cluster centers are initially chosen at random from the
     *  set of input observation vectors, where the probability of choosing
     *  vector x is high if x is NOT near any previously chosen centers.
     */
    def k_means_plus_plus(points: Array[(Float, Float)], k: Int)
            : Array[(Float, Float)] = {

        val n = points.length
        val oracle = new scala.util.Random

        // uniformly pick up the first centroid.
        val first_centroid = points(oracle.nextInt(n))
        println("first centroid: " + first_centroid)

        val centroids = scala.collection.mutable.Set(first_centroid)

        // Find out the nearest centroid, given the point and existing centroids.
        def nearest_centroid(p: (Float, Float)) : (Float, Float) = {
            centroids.foldLeft((Float.MaxValue, (0F, 0F))){ (B, A) =>
                val d = distance(p, A)
                if (d < B._1) (d, A) else B
            }._2
        }

        // Iteractively pick up the rest of centroids,
        //  in favor of 'outlier' points to the existing centroids.
        (1 to k-1).foreach { _ =>
            // calculate the chance to be picked for each point,
            //   which is proportional to the distance of its nearest centroid.
            val prob_dist = points.map{ p =>
                val nc = nearest_centroid(p)
                distance(p, nc)
            }

            val sum = prob_dist.sum
            val lotto = oracle.nextFloat() * sum

            // Find out the point that has been picked.
            var cursor = 0F
            var index = 0
            prob_dist.find{ p =>
                if ( cursor <= lotto && lotto < cursor+p ) true
                else { cursor += p; index += 1; false }
            }

            centroids += points(index)
        }

        // Return the selected centroids.
        centroids.toArray
    }

    /**
     *  cluster the points, given the centroids.
     */
    def cluster(   points: Array[(Float, Float)],
                centroids: Array[(Float, Float)]) = {

        val k = centroids.length
        val clusters = new Array[Set[(Float, Float)]](k)

        // initialize sets, otherwise NULL pointer exception.
        (1 to k).foreach(i=>clusters(i-1) = Set[(Float, Float)]())

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
            // Filter out the empty and comment lines.
            val input =
              Source.fromFile(args(0)).getLines.toList
                .filterNot(l => l.startsWith("#") || l.trim.equals(""))

            val k = args(1).toInt
            
            process(input, k)
            
        } else {
            Console.err.println("Error: missing arguments!")
            println("Usage:\n\t scala K_Mean_Clustering input.file k")
        }
    }

}
