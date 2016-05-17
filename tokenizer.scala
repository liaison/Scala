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


