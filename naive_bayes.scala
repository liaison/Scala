
import scala.io.Source
import scala.collection.mutable.ArrayBuffer

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
   * Load the input file into the training and the testing data set.
   */
  def parse_input(input: List[String]) :
    (List[(String, List[String])], List[List[String]]) = {

    val training_data = new ArrayBuffer[(String, List[String])]()
    val testing_data = new ArrayBuffer[List[String]]()

    input.foreach{ line =>
      val delimit = line.indexOf(',')
      val label = line.take(delimit).trim
      val document = line.substring(delimit+1).trim
      label match {
        case "" => testing_data += parse_term(document)
        case _  => training_data.append((label, parse_term(document)))
      }
    }

    (training_data.toList, testing_data.toList)
  }


  def print_list[T](header: String, list: List[T]) {
    println(header)
    println(list)
  }

  /**
   *  The main function !
   */
  def main(args : Array[String]) {

    if (args.length > 0) { 
      val input = Source.fromFile(args(0)).getLines.toList
                    .filterNot(l => l.startsWith("#") || l.trim == "")

      val (training, testing) = parse_input(input)

      print_list("training_data_set:", training)
      print_list("testing_data_set:", testing)

    } else {
      Console.err.println("Error: missing the input file!")
      println("Usage:\n\t Scala Tokenizer input.file")
    }

  } // end of main

} // end of the singleton object.


