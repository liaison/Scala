
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
    val total_item = training.size.toDouble

    // Calculate the entropy of groups by dividing by each feature.
    val feature_entropy = (0 until feature_size).toList.map{ i =>
      val groups = partition_by(training, i)

      Utils.print_map("partitions:", groups)

      val E_groups = groups.foldLeft(0.0){ case (acc, (feature, subgroup)) =>
        acc + entropy(subgroup) * (subgroup.size / total_item)
      }
      (i, E_groups)
    }

    Utils.print_list("feature_entropy:", feature_entropy)

    // The best feature is the one with the least entropy,
    // Dividing from this feature would allow us to "gain" the most information.
    val best_feature = feature_entropy.sortWith{
      case ((aI, aE), (bI, bE)) => aE < bE
    }.head

    println(best_feature)

  }

  /**
   * Calculate the entropy of a group/set of data.
   *   E(data_set) = sum( - log(prob) * prob )
   *
   *     prob: probability of each value
   */
  def entropy(data_set: List[String]) : Double = {
    val value_count = Map[String, Int]()
    val total_count = data_set.size.toDouble

    data_set.foreach{ value =>
      val count = value_count.getOrElse(value, 0)
      value_count(value) = count + 1
    }

    value_count.foldLeft(0.0){ case (acc, (value, count)) =>
      val prob = count / total_count
      acc - math.log(prob) * prob
    }
  }


  /**
   * Partition a data set based on a specific feature.
   *  @return label_group for each value of the specified feature.
   */
  def partition_by(data_set: Array[(String, Array[String])], feature_index: Int)
    : Map[String, List[String]] = {
    // <feature_value, label_value>
    val partition_map = Map[String, ArrayBuffer[String]]()
 
    data_set.foreach{ case (label, feature_vec) =>
      val key = feature_vec(feature_index)
      val group = partition_map.getOrElse(key, new ArrayBuffer[String]())
      group.append(label)
      partition_map(key) = group
    }

    // convert the group type and return the partition map.
    partition_map.map{ case (label, group) => (label, group.toList)}
  }


  /**
   *  The main function !
   */
  def main(args : Array[String]) {

    if (args.length > 0) { 
      val input = Source.fromFile(args(0)).getLines.toList
                    .filterNot(l => l.startsWith("#") || l.trim == "")

      Utils.print_list("Input:", input, "\n")

      val training = parse(input)

      // partition
      
      // Training
      train(training)

      // Fit model

    } else {
      Console.err.println("Error: missing the input file!")
      println("Usage:\n\t Scala Tokenizer input.file")
    }

  } // end of main

} // end of the singleton object.


