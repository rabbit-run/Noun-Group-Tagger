package edu.nyu.cs.nlp

import java.io._
import opennlp.maxent._
import opennlp.maxent.io._
import scala.io.Source
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.Map
import math._


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

  def predict(modelFileName: String, features: Array[String]): Map[String, Double] = {
    val m = new SuffixSensitiveGISModelReader(
        new File(modelFileName)).getModel()
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
  
  def log_plus(x: Double, y: Double): Double = log(x) + log(y)
  
  // for a specified word, compute the feature vector for this
  // word based on its previous word
  def computeFeature(previous: Array[String], 
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
}