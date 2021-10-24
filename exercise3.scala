package List0

import scala.collection.mutable.{ListBuffer, Map}
import scala.io.Source
import java.io.{File, FileNotFoundException, PrintWriter}

class Text(){
  private var wordCounter: Map[String, Int] = Map()
  var sortedWords = Map[String, Int]().toSeq
  val stopwords = List("ourselves", "hers", "between", "yourself", "but", "again", "there", "about", "once", "during", "out", "very", "having", "with", "they", "own", "an", "be", "some", "for", "do", "its", "yours", "such", "into", "of", "most", "itself", "other", "off", "is", "s", "am", "or", "who", "as", "from", "him", "each", "the", "themselves", "until", "below", "are", "we", "these", "your", "his", "through", "don", "nor", "me", "were", "her", "more", "himself", "this", "down", "should", "our", "their", "while", "above", "both", "up", "to", "ours", "had", "she", "all", "no", "when", "at", "any", "before", "them", "same", "and", "been", "have", "in", "will", "on", "does", "yourselves", "then", "that", "because", "what", "over", "why", "so", "can", "did", "not", "now", "under", "he", "you", "herself", "has", "just", "where", "too", "only", "myself", "which", "those", "i", "after", "few", "whom", "t", "being", "if", "theirs", "my", "against", "a", "by", "doing", "it", "how", "further", "was", "here", "than")

  def CountWords(input:String): Unit = {
    var text = Source.fromFile(input).getLines().mkString(" ")
    var splitedText = text.split(" ")
    var listOfWords = ListBuffer[String]()
    for (word <- splitedText){
      var wordsInLowercase = word.toLowerCase().replaceAll("\\p{Punct}", "")
      if (!stopwords.contains(wordsInLowercase)){
        listOfWords += wordsInLowercase
      }
    }
    for (word <- listOfWords){
      if (wordCounter.contains(word)){
        wordCounter(word) += 1
      }
      else{
        wordCounter += (word -> 1)
      }
    }
  }

  def WordCloud(): Unit = {
    sortedWords = wordCounter.toSeq.sortWith(_._2 > _._2)
    for (s <- sortedWords.take(41)){
      if(s._1!=""){
        println(s._2 + ", " + s._1)
      }
    }
  }

  def SaveWords(path:String, title:String): Unit = {
    val filePath = path+"//"+title+".csv"
    val printWriter = new PrintWriter(new File(filePath))
    for (s<-sortedWords.take(41)){
      if (s._1!=""){
        printWriter.write(s._2 + ", " + s._1 + '\n')
      }
    }
    printWriter.close()
  }
}

object Book {
  def main(args:Array[String]): Unit = {
    var exit: Boolean = true
    var text = new Text()
    while(exit){
      try{
        val input = io.StdIn.readLine("File location: ")
        if (input=="exit"){
          val filePath = io.StdIn.readLine("File path: ")
          val fileTitle = io.StdIn.readLine("File name: ")
          text.SaveWords(filePath, fileTitle)
          exit = false
        }
        else{
          text.CountWords(input)
          text.WordCloud()
        }
      }
      catch{
        case e: FileNotFoundException => println("Wrong path")
      }
    }
  }
}
