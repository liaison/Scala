
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


  class DTree(n: String) {
    val name = n
    val children = new ArrayBuffer[DTree]()
    val data = new ArrayBuffer[(String, Array[String])]()
  }


  def print_dtree(root: DTree) {
    println("decision_tree:")

    def rec_print(node: DTree, level: Int) {
      val indent = Array.fill[Char](level)('\t')
      indent.foreach(print)
      println(node.name)
      node.data.foreach{ case (label, features) =>
        indent.foreach(print)
        print(s"$label: ")
        features.foreach(f => print(s"\t$f,"))
        println("")
      }
      node.children.foreach(c => rec_print(c, level+1))
    }

    rec_print(root, 1)
  }


  def train(training: Array[(String, Array[String])]) {
    val root = divide(training)

    print_dtree(root)
  }

  /**
   *  Classify/divide the data set, according to the decision tree algorithm.
   */
  def divide(data_set: Array[(String, Array[String])]) : DTree = {

    val (label, feature) = data_set(0)
    val feature_size = feature.size
    val total_item = data_set.size.toDouble

    // Calculate the entropy of groups by dividing by each feature.
    val feature_entropy = (0 until feature_size).toList.map{ i =>
      val groups = labels_group_by(data_set, i)

      //Utils.print_map("partitions:", groups)

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

    println(s"best_feature: $best_feature") 

    val (bf_index, bf_entropy) = best_feature

    val node_name = s"feature:$bf_index, entropy:$bf_entropy"
    val node = new DTree(node_name)

    // Recursively divide the data set, start from the best feature
    if (bf_entropy != 0) {
      val partitions = partition_by(data_set, bf_index)
      node.children ++= partitions.map(g => divide(g))
    
    } else {
      // leaf node
      node.data ++= data_set
    }

    // return the node
    return node
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
   * Select labels group by a specific feature.
   *  @return label_group for each value of the specified feature.
   */
  def labels_group_by(data_set: Array[(String, Array[String])],
                      feature_index: Int)
    : Map[String, List[String]] = {
    // <feature_value, label_value>
    val feature_label_map = Map[String, ArrayBuffer[String]]()
 
    data_set.foreach{ case (label, feature_vec) =>
      val key = feature_vec(feature_index)
      val group = feature_label_map.getOrElse(key, new ArrayBuffer[String]())
      group.append(label)
      feature_label_map(key) = group
    }

    // convert the group type and return the partition map.
    feature_label_map.map{ case (f_value, group) => (f_value, group.toList) }
  }


  /**
   * Partition the data set into a list of sub dataset based on the selected feature.
   */
  def partition_by(data_set: Array[(String, Array[String])],
                   feature_index: Int)
    : Array[Array[(String, Array[String])]]= {

    // <feature_value, label_value>
    val partition_map = Map[String, ArrayBuffer[(String, Array[String])]]()

    data_set.foreach{ case (label, feature_vec) =>
      val key = feature_vec(feature_index)
      val group = partition_map.getOrElse(key,
              new ArrayBuffer[(String, Array[String])]())
      group.append((label, feature_vec))
      partition_map(key) = group
    }

    // convert the group type and return the partition map.
    partition_map.values.map{ _.toArray }.toArray
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


