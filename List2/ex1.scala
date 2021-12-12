package List2

import scala.collection.mutable.{ListBuffer, Map, HashMap, Set, MultiMap}
import scala.io.Source
import java.io.{File, FileNotFoundException, PrintWriter}
import scala.io.StdIn.readInt
import scala.io.StdIn.readLine
import scala.collection.immutable.ListMap

//import math.Fractional.Implicits.infixFractionalOps
//import math.Integral.Implicits.infixIntegralOps
//import math.Numeric.Implicits.infixNumericOps

class Graph() {
  private var allVertices: List[Int] = List()
  var mapdata: Map[Int, List[Int]] = Map()
  //var input: String = inputFile

  def collectData (input:String): Unit = {
    mapdata = readFile(input)
    //println(mapdata)
    //println(allVertices)
  }

// ========================================Mapping======================================================

  def readFile(filename: String): Map[Int, List[Int]] = {
    processInput(Source.fromFile(filename).getLines)
  }

  def processInput(lines: Iterator[String]): Map[Int, List[Int]] = {
    var mapBuffer: Map[Int, List[Int]] = Map()
    try {
      for (line <- lines) {
        val splitline = line.split(" ").map(_.trim).toList.map((s: String) => s.toInt)
        //println(splitline)

        for (s <- splitline){
          if (!allVertices.contains(s)){
            var allVerBuffer = allVertices :+ s
            allVertices = allVerBuffer
            //println(s)
          }
        }

        if (mapBuffer.contains(splitline.head)){
          var newBuffer = mapBuffer(splitline.head) ::: splitline.tail
          mapBuffer(splitline.head) = newBuffer
        }
        else{
          mapBuffer = mapBuffer + (splitline.head -> List(splitline.tail.head))
        }
      }
    } catch {
    case ex: Exception => println("Sorry, an exception happened.")
    }
    mapBuffer
  }

// ===============================================Reduce=================================================

  def inDeg(v:Int, map:Map[Int, List[Int]]): Int = {
    if (map.contains(v)){
      return map(v).size
    }
    else{
      return 0
    }
  }

  def outDeg(v:Int, map:Map[Int, List[Int]]): Int = {
    var outDeg = 0
    for (key <- map){
      if (key._2.contains(v)){
        outDeg += 1
      }
    }
    return outDeg
  }

  def graphAnalysis(): Unit = {
    for (n <- allVertices){
      var in = inDeg(n,mapdata)
      var out = outDeg(n,mapdata)
      println("(" + n + ", inDeg(" + n + ") = " + in + " , outDeg(" + n + ") = " + out + "),")
    }
  }
}

object DirectedGraph {
  def main(args:Array[String]): Unit = {
    val inputFile = "input1.txt"
    var graph = new Graph()
    graph.collectData(inputFile)
    graph.graphAnalysis()
  }
}