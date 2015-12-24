
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
     *    Map[url, List[neighbor_url]]
     */
    def parse(input : List[String]) : 
        (Set[String], Map[String, List[String]]) ={
        
        val url_set = Set[String]()
        
        /** Basically, doing .groupByKey() SQL operation to obtain the map: 
                Map[url, List[neighbor_urls]] */
        var url_neighbors = input.map{
            line => val pair = line.split("\\s+")
            (pair(0), pair(1))
        }.groupBy{case (p0, p1) => p0}.map{ case (k, group) =>
            val neighbor_list = group.map{ case (p0, p1) => url_set ++ List(p0, p1); p1 }.toList
            (k, neighbor_list)
        }

        (url_set, Map() ++ url_neighbors)
    }

    /**
     *  Initialize the initial PageRank value (i.e. 1F) for each URL.
     */
    def init_rank(url_set : Set[String])
        : Map[String, Float] = {
   
        val page_rank_map = Map[String, Float]()
        
        // Set the initial PageRank value as 1
        url_set.foreach{ url => page_rank_map + (url -> 1F) }
        
        page_rank_map
    }
    
    def page_ranking(url_map  : Map[String, List[String]],
                     rank_map : Map[String, Float],
                     iter     : Int)
        : Map[String, Float] = {
        
        var new_rank_map = rank_map
        
        for (i <- 0 to iter ) {
            val contribs_list = url_map.flatMap{ case (url, neighbors) =>
                val contrib = new_rank_map(url) * 0.85F / neighbors.length
                neighbors.map( neighbor => (neighbor, contrib) ).toList
            }
    
            print(contribs_list)
            
            new_rank_map = Map() ++
                contribs_list.groupBy{ case (t0, t1) => t0 }.
                map{ case (k, group) => 
                    val value_list = group.map{ case (p0, p1) => p1 }.toList
                    (k, 0.15F + value_list.foldLeft(0F)(_+_))
                }
        
        }

        // Return the new page rank values.
        Map() ++ new_rank_map
    }

    def process(input : List[String]) {
        
        val (url_set, url_neighbors_map) = parse(input)
        
        print(url_neighbors_map)
        
        val page_rank_map = init_rank(url_set)
        
        print("init ranking:")
        print(page_rank_map)
        
        // number of iteration
        val iter = 10
        print(page_ranking(url_neighbors_map, page_rank_map, iter))
    }

    def main(args : Array[String]) {
    
        if (args.length > 0) { 
            val input = Source.fromFile(args(0)).getLines.toList
            
            process(input)
            
        } else {
            Console.err.println("Error: missing the links mapping file!")
        }
    }

}
