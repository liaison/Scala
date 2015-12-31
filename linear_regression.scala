
import scala.io.Source

object LinearRegression {
    
    def parse(input: List[String]) = {
        val yX = input.map{ s =>
            val yX = s.split("\\s+").toList.map(_.toFloat)
            (yX.head, yX.tail)
        }.unzip
        
        val y = yX._1
        val X = yX._2

        println(y)
        println(X)
    }

    def process(input: List[String]) {
        println(input)

        parse(input)
    }

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
}


