/*
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements.  See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License.  You may obtain a copy of the License at
 *
 *    http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
import Utils._

import scala.io.Source
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.Map
import scala.collection.mutable.Set


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
        case "NA" => test += parse_term(document)
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

    // the gobal vacabulary set
    val _global_term_set = Set[String]()

    /**
     * Fit the training data set with multinomial naive Bayes model,
     *  in order to obtain two probabilites:
     *  - priori_probability(label)
     *  - conditional_probability(term | label)
     */
    def fit(training: List[(String, List[String])]) {

      // label: (count_all_terms, Map(term, count))
      // The frequence of a term withinthe documents of a specific category.
      val term_per_label = Map[String, (Int, Map[String, Int])]()

      // count the number of documents for each label.
      val doc_per_label = Map[String, Int]()

      training.foreach{ case (label, document) =>
        // update the map of (label, doc_count)
        val doc_count = doc_per_label.getOrElse(label, 0)
        doc_per_label(label) = doc_count + 1

        val (total_term_cnt, term_freq) =
          term_per_label.getOrElse(label, (0, Map[String, Int]()))

        // update the frequency count for each term
        document.foreach{ term =>
          val term_count = term_freq.getOrElse(term, 0)
          // increment the count
          term_freq(term) = term_count + 1
          // update the vacabulary
          _global_term_set += term
        }

        // update the term_per_label table
        term_per_label(label) = (total_term_cnt + document.size, term_freq)
      } // end of training data set.

      //*calculate the 'a priori' probability for each label/class.
      val total_doc_num = training.size
      doc_per_label.foreach{ case (label, doc_count) =>
        _priori_prob_label(label) = doc_count.toDouble / total_doc_num
      }
      Utils.print_map("label_priori_probability:", _priori_prob_label)

      //*calculate the conditional probability for each combination of label/term
      term_per_label.foreach{ case (label, (total_count, term_per_label)) =>

       _global_term_set.foreach{ case term =>

          val term_count = term_per_label.getOrElse(term, 0)
          // Use the Laplace smoothing to avoid to zero-way 
          //   the rare term due to the 'sparse' property of sampling.
          _cond_prob_term_on_label(s"${label}_${term}") =
            (term_count.toDouble+1) / (total_count + _global_term_set.size)
        }
      }

      Utils.print_map("term_per_label:", term_per_label)
    }


    def print_model() {
      println(s"global_term_set:\n ${_global_term_set}")

      val sorted_cond_prob =
        _cond_prob_term_on_label.toSeq.sortBy{ case (key, _) => key }.toList

      Utils.print_list("label_term_conditional_probability:",
                       sorted_cond_prob, "\n")
    }

    def predict(test: List[List[String]]): List[List[(String, Double)]] = {
        test.map{ doc =>
          _priori_prob_label.map{ case (label, priori)=>
             val ranking = doc.foldLeft(math.log(priori)){ (acc, term) =>
               // If there is any new term from test data that does not appear
               //   in the training data set, then assumits conditional
               //   probability is Zero
               acc +
                 math.log(
                   _cond_prob_term_on_label.getOrElse(s"${label}_${term}", 1.0))
             }
             (label, ranking)
          }.toList.sortWith{ case ((_,rankA), (_, rankB)) => rankA > rankB }
        }
    }

  }


  /**
   * https://en.wikipedia.org/wiki/Naive_Bayes_classifier#Bernoulli_naive_Bayes.
   * Note: the Bernoulli model is NOT equivalent to the multinomial model with
   *   the frequence of term truncated into one.
   * 
   * The probability model of Bernoulli is different from the multinomial,
   *   where it explicitly models the "absence" of a term.
   */
  object Bernoulli_naive_Bayes {
     // priori probability for each class / label
     var _priori_prob = Map[String, Double]()

     // conditional probability for each combination of label/term
     var _cond_prob = Map[(String, String), Double]()
     
     // the gobal set of vacabulary
     val _global_term_set = Set[String]()

     /**
      *  Fit the training data with Bernoulli Naive Bayes model
      */
     def fit(training: List[(String, List[String])]) {
       val total_doc_cnt = training.size

       // the number of documents that each label presents
       val doc_per_label = Map[String, Int]()

       // given a label, count the number of documents that contains the term,
       //  where the key is ($label, $term)
       val doc_per_label_term = Map[(String, String), Int]()

       training.foreach{ case (label, doc) =>
         // update the number of documents that each kind of label is attributed
         val doc_count = doc_per_label.getOrElse(label, 0)
         doc_per_label(label) = doc_count + 1

         // get the Set of terms within a document.
         val doc_term_set = Set[String]() 
         doc.foreach{ case term =>
           doc_term_set += term
           _global_term_set += term
         }

         doc_term_set.foreach{ case term =>
           val key = (label, term)
           val count = doc_per_label_term.getOrElse(key, 0)
           doc_per_label_term(key) = count + 1
         }
       }

       // calculate the priori probability for each class.
       _priori_prob = doc_per_label.map{ case (label, cnt) =>
         (label, cnt.toDouble / total_doc_cnt)
       }

       // calculate the conditional probability for each combination of label/term.
       _cond_prob = doc_per_label.flatMap{ case (label, doc_per_label) =>

           _global_term_set.map{ case term =>
             val doc_per_term = doc_per_label_term.getOrElse((label, term), 0)

             // laplace smoothing as in multinomial model
             ((label, term), (doc_per_term + 1.0)/(doc_per_label + 2.0))
           }
       }

     } // end of fit() function


     def print_model() {
       val sorted_cond_prob =
         _cond_prob.toSeq.sortBy{ case ((label, term), prob) => label }.toList

       Utils.print_map("Bernoulli priori probability:", _priori_prob)
       Utils.print_list("Bernoulli conditional probability:", sorted_cond_prob, "\n")
     }


     def predict(test: List[List[String]]): List[List[(String, Double)]] = {
        test.map{ doc =>
          _priori_prob.map{ case (label, priori)=>
             val ranking =
               _global_term_set.foldLeft(math.log(priori)){ (acc, term) =>
               val cond_prob = _cond_prob.getOrElse((label, term), 1.0)

               if(doc.contains(term)) acc + math.log(cond_prob)
               else                   acc - math.log(cond_prob)
             }

             // get the posteriori possibility for each label.
             (label, ranking)
          }.toList.sortWith{ case ((_,rankA), (_, rankB)) => rankA > rankB }
        }
     }
  }// end of Bernoulli_naive_Bayes object


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
      multinomial_naive_Bayes.print_model()

      // Fit model
      val result = multinomial_naive_Bayes.predict(test)
      Utils.print_list("test_data_set:", test, "\n")
      Utils.print_list("Multinomial Naive Bayes ranking:", result, "\n")

      Bernoulli_naive_Bayes.fit(training)
      Bernoulli_naive_Bayes.print_model()
      Utils.print_list("Bernoulli result:",
                       Bernoulli_naive_Bayes.predict(test), "\n")

    } else {
      Console.err.println("Error: missing the input file!")
      println("Usage:\n\t Scala Tokenizer input.file")
    }

  } // end of main

} // end of the singleton object.


