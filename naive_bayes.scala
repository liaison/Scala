
import scala.io.Source


/**
 *  Classification. 
 *  Naive Bayes is a simple multiclass classification algorithm,
 *   which is based on the assumption that each variable within
 *   the feature vector is independent from each other.
 *
 *  Built on top of this assumption, Naive Bayes performs a 
 *   maximum likelihood estimate (MLE) which calculates the posteriori
 *   probablity of an Event(the label/categority of a document) with
 *   the priori probability of each class/category and the conditional
 *   probability of each term (word).
 *
 * Reference:
 * [http://nlp.stanford.edu/IR-book/html/htmledition/naive-bayes-text-classification-1.html]
 */
object NaiveBayes {

  /**
   *  Print each line with its line number.
   */
  def printLine(count : Int, line : String) : Int = {
    print(count + "\t"); println(line)
    // Return the next line number.
    count + 1
  }


  /**
   *  Parse the document into a list of words/tokens.
   */
  def parse_term(doc: String) : List[String] = {
    // parse words seperated by \n or spaces.
    // non-alphabetnumeric 
    val punct = "[^a-zA-Z0-9]".r
    val words = doc.split("\\s+").map{w=> punct.replaceAllIn(w, "")}
    // return the words.
    words.toList
  }


  /**
   * Load the input file into the training and testing data set.
   */
  def parse_input(input: List[String]) = {
  
  }

  /**
   *  The main function !
   */
  def main(args : Array[String]) {

    if (args.length > 0) { 
      val input = Source.fromFile(args(0)).getLines.toList
                    .filterNot(l => l.startsWith("#") || l.trim == "")

      parse_input(input)

    } else {
      Console.err.println("Error: missing the input file!")
      println("Usage:\n\t Scala Tokenizer input.file")
    }

  } // end of main

} // end of the singleton object.


