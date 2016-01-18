
import Utils._

import scala.io.Source
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.Map


/**
 *
 */
object DecisionTree {

  def parse(input: List[String]): Array[(String, Array[String])] ={
    input.map{line =>
      val fields = line.split(",").map(_.trim)
      val label = fields(0)
      (label, fields.tail)
    }.toArray
  }


  def train(training: Array[(String, Array[String])]) {

    val (label, feature) = training(0)
    val feature_size = feature.size
   
    //partition(training, index)
  }

  /**
   * Partition a data set based on a specific feature.
   */
  def partition_by(data_set: Array[(String, Array[String])], feature_index: Int)
    : Map[String, Array[(String, Array[String])]] = {
    val partition_map = Map[String, ArrayBuffer[(String, Array[String])]]()
 
    data_set.foreach{ case (label, feature_vec) =>
      val key = feature_vec(feature_index)
      val group = partition_map.getOrElse(key,
                    new ArrayBuffer[(String, Array[String])]())
      group.append((label, feature_vec))
      partition_map(key) = group
    }

    partition_map.map{ case (label, group) => (label, group.toArray)}
  }


  /**
   *  The main function !
   */
  def main(args : Array[String]) {

    if (args.length > 0) { 
      val input = Source.fromFile(args(0)).getLines.toList
                    .filterNot(l => l.startsWith("#") || l.trim == "")

      val training = parse(input)

      Utils.print_array("training", training, "\n")

      // partition
      
      // Training

      // Fit model

    } else {
      Console.err.println("Error: missing the input file!")
      println("Usage:\n\t Scala Tokenizer input.file")
    }

  } // end of main

} // end of the singleton object.


