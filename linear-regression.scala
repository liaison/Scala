
import scala.io.Source

object LinearRegression {
    
    def parse(input: List[String]) = {
        val nums = input.map{ s =>
            val yX = s.split("\\s+")
            (yX.head.toFloat, yX.tail.map(_.toFloat))
        }
        
        println(nums)
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


