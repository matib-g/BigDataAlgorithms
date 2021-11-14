package List0

import scala.collection.mutable.{ListBuffer, Map}
import scala.io.Source
import java.io.{File, FileNotFoundException, PrintWriter}

class Text(){
  private var wordCounter: Map[String, Int] = Map()
  private var allWordCounter: Map[String, Int] = Map()
  private var allDocsTFIDF: Map[String, Map[String, Double]] = Map()
  private var allWordTFIDF: Map[String, Double] = Map()
  var sortedWords = Map[String, Int]().toSeq
  var sortedAllWords = Map[String, Int]().toSeq
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
      if (allWordCounter.contains(word)){
        allWordCounter(word) += 1
      }
      else{
        allWordCounter += (word -> 1)
      }
    }
  }

  def WordCloud(n:Int): Unit = {
    sortedWords = wordCounter.toSeq.sortWith(_._2 > _._2)
    for (s <- sortedWords.take(n)){
      if(s._1!=""){
        println(s._2 + ", " + s._1)
      }
    }
  }

  def WordCloudAll(n:Int): Unit = {
    sortedAllWords = allWordCounter.toSeq.sortWith(_._2 > _._2)
    for (s <- sortedAllWords.take(n)){
      println("All words:")
      if(s._1!=""){
        println(s._2 + ", " + s._1)
      }
    }
  }

  def TFIDFmap(name:String): Unit = {
    if (allWordTFIDF.contain(name)==False){

    }
  }

  def TFIDF(): Unit = {
    
  }

  def SaveWords(path:String, title:String): Unit = {
    val filePath = path+"//"+title+".csv"
    val printWriter = new PrintWriter(new File(filePath))
    for (s<-sortedWords.take(40)){
      if (s._1!=""){
        printWriter.write(s._2 + ", " + s._1 + '\n')
      }
    }
    printWriter.close()
  }

  def SaveAllWords(path:String, title:String): Unit = {
    val filePath = path+"//"+title+".csv"
    val printWriter = new PrintWriter(new File(filePath))
    for (s<-sortedAllWords.take(40)){
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
    val filePath = io.StdIn.readLine("File path (to save): ")
    while(exit){
      try{
        val input = io.StdIn.readLine("Input file location: ")
        val fileTitle = io.StdIn.readLine("File name: ")
        if (input=="exit"){
          val fileTitle = io.StdIn.readLine("Output allword-file name: ")
          text.SaveAllWords(filePath, fileTitle)
          exit = false
        }
        else{
          var n = io.StdIn.readLine("Number of most frequently words to show: ")
          text.CountWords(input)
          text.WordCloud(n.toInt)
          text.WordCloudAll(n.toInt)
          text.SaveWords(filePath, fileTitle)
        }
      }
      catch{
        case e1: FileNotFoundException => println("Wrong path")
      }
    }
  }
}
