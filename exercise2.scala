package List0

import scala.collection.mutable.{ListBuffer, Map}
import scala.io.Source

object Book {
  def main(args:Array[String]): Unit = {
    val stopwords = List("ourselves", "hers", "between", "yourself", "but", "again", "there", "about", "once", "during", "out", "very", "having", "with", "they", "own", "an", "be", "some", "for", "do", "its", "yours", "such", "into", "of", "most", "itself", "other", "off", "is", "s", "am", "or", "who", "as", "from", "him", "each", "the", "themselves", "until", "below", "are", "we", "these", "your", "his", "through", "don", "nor", "me", "were", "her", "more", "himself", "this", "down", "should", "our", "their", "while", "above", "both", "up", "to", "ours", "had", "she", "all", "no", "when", "at", "any", "before", "them", "same", "and", "been", "have", "in", "will", "on", "does", "yourselves", "then", "that", "because", "what", "over", "why", "so", "can", "did", "not", "now", "under", "he", "you", "herself", "has", "just", "where", "too", "only", "myself", "which", "those", "i", "after", "few", "whom", "t", "being", "if", "theirs", "my", "against", "a", "by", "doing", "it", "how", "further", "was", "here", "than")
    val text = Source.fromFile("/Users/mateuszbulanda-gorol/Desktop/Studia/BigDataAlgorithms/List0/Catch-22.txt").getLines().mkString(" ")
    var splitedText = text.split(" ")
    var listOfWords = ListBuffer[String]()
    for (words <- splitedText){
      var wordsInLowercase = word.toLowerCase().replaceAll("\\p{Punct}", "")
      if (!stopwords.contains(wordsInLowercase)){
        listOfWords += wordsInLowercase
      }
    }
    val wordCounter :Map [String, Int] = Map()
    for (word <- listOfWords){
      if (wordCounter.contains(word)){
        wordCounter(word) += 1
      }
      else{
        wordCounter += (word -> 1)
      }
    }
    val sortedMap = wordCounter.toSeq.sortWith(_._2 > _._2)
    for (s <- sortedMap.take(41)){
      if(s._1!=""){
        println(s._2 + ", " + s._1)
      }
    }
  }
}
