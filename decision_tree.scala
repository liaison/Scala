
import Utils._

import scala.io.Source
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.Map


/**
 *
 */
object DecisionTree {

  /**
   *  The main function !
   */
  def main(args : Array[String]) {

    if (args.length > 0) { 
      val input = Source.fromFile(args(0)).getLines.toList
                    .filterNot(l => l.startsWith("#") || l.trim == "")

      // partition
      
      // Training

      // Fit model

    } else {
      Console.err.println("Error: missing the input file!")
      println("Usage:\n\t Scala Tokenizer input.file")
    }

  } // end of main

} // end of the singleton object.


