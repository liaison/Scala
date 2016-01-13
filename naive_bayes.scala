
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
   * Fit the training data set with multinomial naive Bayes model.
   *
   *  (priori_probability(label), conditional_probability(term | label))
   */
  def multinomial_naive_bayes_fit(training: List[(String, List[String])])
    : Map[String, Double] = {

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

    // conditional_probability for each combination of label and term
    val cond_prob_label_term = Map[String, Double]()

    label_term_freq.foreach{ case (label, (total_count, term_freq))  =>
      val term_num = term_freq.size
      term_freq.map{ case (term, count) =>
        // Use the Laplace smoothing to avoid to zero-way 
        //   the rare term due to the 'sparse' property of sampling.
        cond_prob_label_term(s"${label}_${term}") =
          (count.toDouble+1) / (total_count + term_num)
      }
    }

    val total_doc_num = training.size
    // calculate the 'a priori' probability for each label/class.
    val priori_prob_label = label_doc_count.map{ case (label, doc_count) =>
      (label, doc_count.toDouble / total_doc_num)
    }

    Utils.print_map("label_term_conditional_probability:", cond_prob_label_term)
    cond_prob_label_term
  }


  /**
   *  The main function !
   */
  def main(args : Array[String]) {

    if (args.length > 0) { 
      val input = Source.fromFile(args(0)).getLines.toList
                    .filterNot(l => l.startsWith("#") || l.trim == "")

      val (training, test) = parse_input(input)

      Utils.print_list("training_data_set:", training, "\n")
      Utils.print_list("test_data_set:", test)

      multinomial_naive_bayes_fit(training)

    } else {
      Console.err.println("Error: missing the input file!")
      println("Usage:\n\t Scala Tokenizer input.file")
    }

  } // end of main

} // end of the singleton object.


