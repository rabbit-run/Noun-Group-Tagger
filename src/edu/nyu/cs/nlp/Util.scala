package edu.nyu.cs.nlp

import java.io._
import opennlp.maxent._
import opennlp.maxent.io._
import scala.io.Source
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.Map
import math._
import opennlp.model.AbstractModel


object Util {
  // use PrintWriter to write to file
  def printToFile(f: java.io.File)(op: java.io.PrintWriter => Unit) {
    val p = new java.io.PrintWriter(f)
    try { op(p) } finally { p.close() }
  }
  
  // Flatten a list of lists
  def flatten[T](list : List[List[T]]) = for(xs <- list; x <- xs) yield x

  def trainModel(dataFileName: String, modelFileName: String): Unit = {
    try {
      val datafr = new FileReader(new File(dataFileName))
      val es = new BasicEventStream(new PlainTextByLineDataStream(datafr))
      val model = GIS.trainModel(es, 100, 4)
      val outputFile = new File(modelFileName)
      val writer = new SuffixSensitiveGISModelWriter(model, outputFile)
      writer.persist()
    } catch {
      case e: Exception => {
        System.out.print("Unable to create model due to exception: ")
        System.out.println(e)
      }
    }
  }

  def predict(m: AbstractModel, features: Array[String]): Map[String, Double] = {
    val probs = m.eval(features)
    val prob_map = Map.empty[String, Double]
    for ( i <- 0 until 3) {
      prob_map(m.getOutcome(i)) = probs(i)
    }
    prob_map
  }
  
  // Read the file, split the sentences
  def split_sentences(fileName: String): List[List[Array[String]]] = {
    val sentences = new ListBuffer[List[Array[String]]]
    	val sent = new ListBuffer[Array[String]]
    val end_of_sent = Array("<f>", "<f>", "<f>")
    
    for (line <- Source.fromFile(fileName).getLines()) {
      if (!line.stripLineEnd.isEmpty()) {
        val word = line.stripLineEnd.split("\t")
        sent.append(word)
      } else {
        sent.append(end_of_sent)
        sentences.append(sent.toList)
        sent.clear()
      }
    }
    sentences.toList
  }
  
  def isCapitalized(word: String): Boolean = {
    if (word(0).isUpper && word.tail.forall(_.isLower)) {
      return true
    }
    return false
  }
  
  def isMonth(word: String): Boolean = {
    val months = Set("January", "February", "March", "April", "May",
        "June", "July", "August", "September", "October", "November",
        "December")
    if (months.contains(word)) {
      return true
    }
    return false
  }
  
  // for a specified word, compute the feature vector for this
  // word based on its previous word
  def computeFeature(previous: Array[String], 
      current: Array[String], next:Array[String]): String = {
    val featureVector = new StringBuffer
    
    if (previous(0) == "<s>") {
      featureVector.append("firstWord=").append("true").append(" ")
    }
    else {
      featureVector.append("firstWord=").append("false").append(" ")
    }
    
    featureVector.append("previousNounGroupTag=").
    		append(previous(2)).append(" ")
    featureVector.append("previousPOSTag=").append(previous(1)).append(" ")
    featureVector.append("previousWord=").append(previous(0)).append(" ")
    featureVector.append("currentPOSTag=").append(current(1)).append(" ")
    featureVector.append("currentWord=").append(current(0)).append(" ")
    featureVector.append("nestPOSTag=").append(next(1)).append(" ")
    featureVector.append("nextWord=").append(next(0)).append(" ")
    
    if(isCapitalized(current(0))) 
      featureVector.append("capitalized=").append("true").append(" ")
    else 
      featureVector.append("capitalized=").append("false").append(" ")
     
    // At last, append the label
    featureVector.append(current(2)).append("\n")
    featureVector.toString()
  }
}