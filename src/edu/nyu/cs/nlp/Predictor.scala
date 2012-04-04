package edu.nyu.cs.nlp

import edu.nyu.cs.nlp.Util._
import opennlp.maxent._
import opennlp.maxent.io._
import scala.collection.mutable.Map
import math._
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.ArrayBuffer

class Predictor (testFileName: String, modelFileName: String){
  private val states = Set("B-NP", "I-NP", "O")
  
  def go(): Unit = {
    val sentences = split_sentences(testFileName)
    
    //test
    this.decode(sentences(0))
//    for (sent <- sentences) {
//      this.decode(sent)
//    }
  }
  
  def compute_prob(previous: Array[String], current: Array[String])
  	  : Map[String, Double] = {
    val features = computeFeature(previous, current).stripLineEnd.split(" ")
    predict(modelFileName, features)
  }
  
  def assume(word: Array[String], assumedLabel: String): Array[String] = {
    Array(word(0), word(1), assumedLabel)
  }
  
  def decode(sent: List[Array[String]]): Unit = {
    val viterbi = Array.empty[Map[String, Double]]
     // Array.tabulate(3, sent.length + 2)((x, y) => 0.0)
    val backPointer = Array.tabulate(3, sent.length + 2)((x,y) => 0)
    val labels = Set("B-NP", "I-NP", "O")
    val start_state = Array("<s>","<s>","<s>")
    val path = Map.empty[String, ListBuffer[String]]
    
    // initialize
    val init_probs = compute_prob(sent(0), start_state)
    for (l <- labels) {
      viterbi(0)(l) = init_probs(l)
      path(l) = ListBuffer(l)
    }
    
    // forward pass
    for (i_token <- 1 until sent.length) {
      val previous = sent(i_token -1)
      val current = sent(i_token)
      val prob_map = Map.empty[String, Map[String, Double]]
    	  
      for (previous_label <- labels) {
        val probs = compute_prob(assume(previous, previous_label), current)
        for (predict_label <- labels) {
          val prob_to_here = log_plus(probs(predict_label), 
              viterbi(i_token-1)(previous_label))
          prob_map(predict_label)(previous_label) = prob_to_here
        }
      }
      
      for (l <- labels) {
        val best_result = prob_map(l).maxBy(_._2)
        
        // TODO add this to viterbi, and record the back pointer
      }
    	}
    
    
  }
  
  
}