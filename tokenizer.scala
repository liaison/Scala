
import scala.io.Source


/**
 *  A tokenizer that extracts words from a text,
 *   and do some processing to build features for later learning.
 */
object Tokenizer {

  /**
   *  Print each line with its line number.
   */
  def printLine(count : Int, line : String) : Int = {
    print(count + "\t"); println(line)
    // Return the next line number.
    count + 1
  }


  /**
   *  Parse the input file into a list of words/tokens without punctuation.
   */
  def parse(input: List[String]) : List[String] = {
      // parse words seperated by \n or spaces.
      
      // non-alphabetnumeric 
      val punct = "[^a-zA-Z0-9]".r
      val words = input.flatMap{line =>
        line.split("\\s+").map{w=> punct.replaceAllIn(w, "")}
      }
      
      words
  }


  /**
   *  The main function !
   */
  def main(args : Array[String]) {

    if (args.length > 0) { 
      val input = Source.fromFile(args(0)).getLines.toList
                    .filterNot(l => l.startsWith("#") || l.trim == "")

      parse(input).foreach(println)

    } else {
        Console.err.println("Error: missing the input file!")
        println("Usage:\n\t Scala Tokenizer input.file")
    }

  } // end of main

} // end of the singleton object.


