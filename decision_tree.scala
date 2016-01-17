
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


