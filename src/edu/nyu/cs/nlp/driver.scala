package edu.nyu.cs.nlp

import edu.nyu.cs.nlp.Util._


object driver {
  def log2(x: Double) = scala.math.log(x)/scala.math.log(2)
  
  // Main method
  def main(args: Array[String]): Unit = {
   
    //val trainer = new Trainer("train.np")    
    //trainer.train()
    val predictor = new Predictor("dev.np", "trainModel.txt")
    predictor.go()
    
  }
}