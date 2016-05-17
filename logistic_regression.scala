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
import scala.collection.mutable.ListBuffer

/**
 *
 */
object LogisticRegression {

    def main(args: Array[String]) {
       
        if (args.length > 0) {
            val input = Source.fromFile(args(0)).getLines.toList
                              .filterNot{l => l.startsWith("#") || l.trim == ""}
            process(input)
        } else {
            Console.err.println("Missing arguments!")
            println("Usage:\n\t")
        }
    }

    def process(input: List[String]) {
    }

    /*
     * Parse the input into a tuple of (y, X)
     *  where y is list of values for the dependent variable,
     *  and X is a list of vector of the values of the independent variable.
     */
    def parse(input: List[String]) : (List[Float], List[List[Float]]) = {
        input.map{ s =>
            val yX = s.split(",").map(_.trim.toFloat).toList
            (yX.head, yX.tail)
        }.unzip
    }


}


