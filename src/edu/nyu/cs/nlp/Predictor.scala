package edu.nyu.cs.nlp

import edu.nyu.cs.nlp.Util._
import opennlp.maxent._
import opennlp.maxent.io._
import scala.collection.mutable.Map
import math._
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.ArrayBuffer
import java.io.File

class Predictor (testFileName: String, modelFileName: String){
  private val states = Set("B-NP", "I-NP", "O")
  private val model = new SuffixSensitiveGISModelReader(
        new File(modelFileName)).getModel()
  def go(): Unit = {
    val sentences = split_sentences(testFileName)
    val output = ListBuffer.empty[String]
   
    for (sent <- sentences) {
      val result = this.decode(sent)
      
      for (index <- 0 until sent.length-1) {
        output.append((sent(index) :+ result(index)).mkString(" "))
        output.append("\n")
      }
      output.append("\n")
    }
    
    val name_stem = this.testFileName.split('.')(0)
    printToFile(new java.io.File(name_stem+".prediction")){
      p => output.foreach(p.print)
    }
  }
  
  private def compute_prob(previous: Array[String], current: Array[String],
      next:Array[String]): Map[String, Double] = {
    val features = computeFeature(previous, current, next).stripLineEnd.split(" ")
    predict(model, features)
  }
  
  private def assume(word: Array[String], assumedLabel: String): Array[String] = {
    Array(word(0), word(1), assumedLabel)
  }
  
  
  /**
   * Deocode the observed sentence
   */
  private def decode(sent: List[Array[String]]): List[String] = {
    val viterbi = Array.fill(
        sent.length)(Map.empty[String, Double])
    val labels = Set("B-NP", "I-NP", "O")
    val start_state = Array("<s>","<s>","<s>")
    var path = Map.empty[String, ListBuffer[String]]
    
    // initialize
    val init_probs = compute_prob(sent(0), start_state, sent(1))
    for (l <- labels) {
//      if (l == "I-NP") {
//        viterbi(0)(l) = Double.MinValue
//      }
//      else 
      {
        viterbi(0)(l) = log(init_probs(l))
        path(l) = ListBuffer(l)
      }
      
    }
    
    // forward pass
    for (i_token <- 1 until sent.length-1) {
      val previous = sent(i_token -1)
      val current = sent(i_token)
      val next = sent(i_token + 1)
      val prob_map = Map.empty[String, Map[String, Double]]
    	  var newPath  = Map.empty[String, ListBuffer[String]]
      
      for (previous_label <- labels) {
        val probs = compute_prob(assume(previous, previous_label), current, next)
        for (predict_label <- labels) {
          val prob_to_here = log(probs(predict_label)) +  
              viterbi(i_token-1)(previous_label)
              
          if (!prob_map.contains(predict_label)) {
            prob_map(predict_label) = Map.empty[String, Double]
          }
          prob_map(predict_label) += (previous_label -> prob_to_here)
        }
      }
      
      for (l <- labels) {
        val best_result = prob_map(l).maxBy(_._2)
        
        viterbi(i_token)(l) = best_result._2
        newPath(l) = path(best_result._1).clone() += l
      }
      path = newPath.clone()
    	}
    
    val (label, prob) = viterbi(sent.length-2).maxBy(_._2)
    return path(label).toList
  }
}