package edu.nyu.cs.nlp
import scala.io.Source
import scala.collection.mutable.ListBuffer

class FeatureGenerator (fileName: String) {
  
  // Read the file, split the sentences
  def split_sentences(): List[List[Array[String]]] = {
    val sentences = new ListBuffer[List[Array[String]]]
    	val sent = new ListBuffer[Array[String]]
    
    for (line <- Source.fromFile(fileName).getLines()) {
      if (!line.stripLineEnd.isEmpty()) {
        val word = line.stripLineEnd.split("\t")
        sent.append(word)
      } else {
        sentences.append(sent.toList)
        sent.clear()
      }
    }
    sentences.toList
  }
  
  // for a specified word, compute the feature vector for this
  // word based on its previous word
  def compute(previous: Array[String],
      current: Array[String]): String = {
    val featureVector = new StringBuffer
    
    if (previous(0) == "<s>") {
      featureVector.append("firstWord").append(" ")
    }
    featureVector.append("previousNounGroupTag=").
    		append(previous(2)).append(" ")
    featureVector.append("previousPOSTag=").append(previous(1)).append(" ")
    featureVector.append("previousWord=").append(previous(0)).append(" ")
    featureVector.append("currentPOSTag=").append(current(1)).append(" ")
    featureVector.append("currentWord=").append(current(0)).append(" ")
    
    featureVector.append(current(2)).append("\n")
    featureVector.toString()
  }
  
  // compute the features of a specified sentence
  def computeFeature(previous: Array[String],
      words: List[Array[String]]): List[String] = {
    if (words.isEmpty) Nil
    else compute(previous, words.head)::computeFeature(words.head, words.tail)
  }

  def getFeatureVectors(): List[List[String]] = {
    val sentences = this.split_sentences()
    val start_state = Array("<s>","<s>","<s>")
    val featureVectors =
      for {
        sent <- sentences
        features = this.computeFeature(start_state,sent)
      } yield features
    return featureVectors
  }
}