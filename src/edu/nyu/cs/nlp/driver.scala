package edu.nyu.cs.nlp

import edu.nyu.cs.nlp.Util._
import java.util.Calendar


object driver {
  def log2(x: Double) = scala.math.log(x)/scala.math.log(2)
  
  // Main method
  def main(args: Array[String]): Unit = {
    val c1 = Calendar.getInstance();
    
    val testFileName = args(0)
    val trainer = new Trainer("train.np")    
    trainer.train()
    val predictor = new Predictor(testFileName, "trainModel.txt")
    println("running")
    predictor.go()
    println("finish!")
    
    val c2 = Calendar.getInstance();
    System.out.println("time:" + (c2.getTimeInMillis() - c1.getTimeInMillis())/1000.0 + "s");
  }
}