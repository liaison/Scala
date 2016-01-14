
import Utils._

import scala.io.Source
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.Map


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
   * Load the input file into the training and the test data set.
   */
  def parse_input(input: List[String]) :
    (List[(String, List[String])], List[List[String]]) = {

    val training = new ArrayBuffer[(String, List[String])]()
    val test = new ArrayBuffer[List[String]]()

    input.foreach{ line =>
      val delimit = line.indexOf(',')
      val label = line.take(delimit).trim
      val document = line.substring(delimit+1).trim
      label match {
        case "" => test += parse_term(document)
        case _  => training.append((label, parse_term(document)))
      }
    }

    (training.toList, test.toList)
  }


  /**
   * The class/object to do Naive Bayes estimate.
   */
  object multinomial_naive_Bayes {

    //Note: The internal attributes start with a _

    // The 'priori' probability for each label/calss.
    val _priori_prob_label = Map[String, Double]()

    // conditional_probability for each combination of term given label.
    val _cond_prob_term_on_label = Map[String, Double]()


    /**
     * Fit the training data set with multinomial naive Bayes model,
     *  in order to obtain two probabilites:
     *  - priori_probability(label)
     *  - conditional_probability(term | label)
     */
    def fit(training: List[(String, List[String])]) {

      // label: (count_all_terms, Map(term, count))
      // The frequence of a term withinthe documents of a specific category.
      val label_term_freq = Map[String, (Int, Map[String, Int])]()

      // count the number of documents for each label.
      val label_doc_count = Map[String, Int]()

      training.foreach{ case (label, document) =>
        // update the map of (label, doc_count)
        val doc_count = label_doc_count.getOrElse(label, 0)
        label_doc_count(label) = doc_count + 1

        val (total_term_cnt, term_freq) =
          label_term_freq.getOrElse(label, (0, Map[String, Int]()))

        // update the frequency count for each term
        document.foreach{ term =>
          val term_count = term_freq.getOrElse(term, 0)
          // increment the count
          term_freq(term) = term_count + 1
        }

        // update the label_term_frequency table
        label_term_freq(label) = (total_term_cnt + document.size, term_freq)
      } // end of training data set.

      Utils.print_map("label_term_frequency:", label_term_freq)

      //*calculate the conditional probability of each term within the given doc
      label_term_freq.foreach{ case (label, (total_count, term_freq))  =>
        val term_num = term_freq.size
        term_freq.map{ case (term, count) =>
          // Use the Laplace smoothing to avoid to zero-way 
          //   the rare term due to the 'sparse' property of sampling.
          _cond_prob_term_on_label(s"${label}_${term}") =
            (count.toDouble+1) / (total_count + term_num)
        }
      }

      val total_doc_num = training.size

      //*calculate the 'a priori' probability for each label/class.
      label_doc_count.foreach{ case (label, doc_count) =>
        _priori_prob_label(label) = doc_count.toDouble / total_doc_num
      }
      Utils.print_map("label_priori_probability:", _priori_prob_label)

      Utils.print_map("label_term_conditional_probability:",
                      _cond_prob_term_on_label)
    }


    def predict(test: List[List[String]]): List[List[(String, Double)]] = {
        test.map{ doc =>
          _priori_prob_label.map{ case (label, priori)=>
             // probability + 1 to shift the value to positive zone.
             //  Otherwise the appearance of evidence would weaken the decision.
             val posteriori = doc.foldLeft(math.log(priori+1)){ (acc, term) =>
               acc +
                 math.log(1 +
                   _cond_prob_term_on_label.getOrElse(s"${label}_${term}", 0.0))
             }

             (label, posteriori)
          }.toList
        }
    }

  }


  /**
   *  The main function !
   */
  def main(args : Array[String]) {

    if (args.length > 0) { 
      val input = Source.fromFile(args(0)).getLines.toList
                    .filterNot(l => l.startsWith("#") || l.trim == "")

      // partition
      val (training, test) = parse_input(input)
      
      // Training
      Utils.print_list("training_data_set:", training, "\n")
      multinomial_naive_Bayes.fit(training)

      // Fit model
      val result = multinomial_naive_Bayes.predict(test)
      Utils.print_list("test_data_set:", test, "\n")
      Utils.print_list("posteriori_prob:", result, "\n")

    } else {
      Console.err.println("Error: missing the input file!")
      println("Usage:\n\t Scala Tokenizer input.file")
    }

  } // end of main

} // end of the singleton object.


