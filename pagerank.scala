
import scala.io.Source
import scala.collection.immutable.List

import scala.collection.mutable.Map
import scala.collection.mutable.Set

/**
 *  Implementation of PageRank alogorithm,
 *    with the damping factor as 0.85, 
 *      i.e. the possibility that a surfer randomly clicks on a page.
 *
 *  Input file format:
 *      URL  outbound_URL
 *
 *  The two URLs are separated by spaces.
 *
 */
object PageRank {

    /**
     *  Parse the input, and construct the essential data structure 
     *      which would be used for the later iteration.
     *
     *    Set[url]
     *    List[url, List[neighbor_url]]
     */
    def parse(input : List[String]) : 
        (Set[String], List[(String, List[String])]) ={
        
        var url_set = Set[String]()
        
        /** Basically, doing .groupByKey() SQL operation to obtain the map: 
                Map[url, List[neighbor_urls]] */
        var url_neighbors = input.map{
            line => val pair = line.split("\\s+")
            (pair(0), pair(1))
        }.groupBy{case (p0, p1) => p0}.map{ case (k, group) =>
            val neighbor_list = group.map{
                case (p0, p1) => url_set ++= List(p0, p1); p1 }.toList
            (k, neighbor_list)
        }

        (url_set, url_neighbors.toList)
    }

    /**
     *  Initialize the initial PageRank value (i.e. 1F) for each URL.
     */
    def init_rank(url_set : Set[String])
        : Map[String, Float] = {
   
        val page_rank_map = Map[String, Float]()
        
        // Set the initial PageRank value as 1
        url_set.foreach{ url => page_rank_map += (url -> 1F) }
        
        page_rank_map
    }
    
    def page_ranking(url_neighbors_list : List[(String, List[String])],
                     rank_map : Map[String, Float],
                     iter     : Int)
        : Map[String, Float] = {
        
        var new_rank_map = rank_map
        val init_values = rank_map.map{
            case (url, rank) => (url, 0.15F / rank_map.size) }.toList

        for (i <- 0 to iter ) {
            val contribs_list = url_neighbors_list.flatMap{ case (url, neighbors) =>
                val contrib = new_rank_map(url) * 0.85F / neighbors.length
                neighbors.map( neighbor => (neighbor, contrib)).toList
            }
            
            // accumulate the contributions and the initial values.
            new_rank_map = Map() ++
                (init_values ::: contribs_list).groupBy{ case (t0, t1) => t0 }.
                map{ case (k, group) => 
                    val value_list = group.map{ case (p0, p1) => p1 }.toList
                    (k, value_list.foldLeft(0F)(_+_))
                }
            
            println("page_rank", new_rank_map) 
        }

        // Return the new page rank values.
        Map() ++ new_rank_map
    }

    def process(input : List[String], iter : Int) {
        
        val (url_set, url_neighbors_list) = parse(input)
        
        println("url_set:")
        println(url_set)
        
        println("url_neighbor_list:")
        println(url_neighbors_list)
        
        val page_rank_map = init_rank(url_set)
        
        println("init ranking:")
        println(page_rank_map)
        
        val final_rank = page_ranking(url_neighbors_list, page_rank_map, iter)
	println("final ranking:")
	println(final_rank)
    }

    def main(args : Array[String]) {
    
        if (args.length > 1) { 
            val input =
              Source.fromFile(args(0)).getLines.toList.filter(_.startsWith("#") == false)
            val iter = args(1).toInt
            process(input, iter)
            
        } else {
            Console.err.println("Error: missing arguments!")
        }
    }

}
