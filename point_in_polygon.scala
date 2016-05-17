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


/**
 *  The algorithm to decide whether a point resides within a polygon
 *    (convex or not).
 */
object PointInPolygon {

  def parse(input: List[String]): List[(Double, Double)] ={
    input.map{line =>
      val coord = line.split("\\s")
      val (x, y) = (coord(0), coord(1))
      (x.toDouble, y.toDouble)
    }
  }


  /**
   *  The main function
   */
  def main(args : Array[String]) {

    if (args.length > 0) { 
      val input = Source.fromFile(args(0)).getLines.toList
                    .filterNot(l => l.startsWith("#") || l.trim == "")

      Utils.print_list("Input:", input, "\n")


    } else {
      Console.err.println("Error: missing the input file!")
      println("Usage:\n\t Scala point_in_polygon.scala input.file")
    }

  } // end of main

} // end of the singleton object.


