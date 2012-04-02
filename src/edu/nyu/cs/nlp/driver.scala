package edu.nyu.cs.nlp

import java.io._
import opennlp.maxent._
import opennlp.maxent.io._

object driver {
  // use PrintWriter to write to file
  def printToFile(f: java.io.File)(op: java.io.PrintWriter => Unit) {
    val p = new java.io.PrintWriter(f)
    try { op(p) } finally { p.close() }
  }
  
  // Flatten a list of lists
  def flatten[T](list : List[List[T]]) = for(xs <- list; x <- xs) yield x

  def trainModel(dataFileName: String, modelFileName: String): Unit = {
    try {
      val datafr = new FileReader(new File(dataFileName));
      val es = new BasicEventStream(new PlainTextByLineDataStream(datafr));
      val model = GIS.trainModel(es, 100, 4);
      val outputFile = new File(modelFileName);
      val writer = new SuffixSensitiveGISModelWriter(model, outputFile);
      writer.persist();
    } catch {
      case e: Exception => {
        System.out.print("Unable to create model due to exception: ");
        System.out.println(e);
      }
    }
  }
  // Main method
  def main(args: Array[String]): Unit = {
   
    val f = new FeatureGenerator("test1.txt")    
    val vectors = f.getFeatureVectors()
    
    printToFile(new File("test1.dat")){
      p => flatten(vectors).foreach(p.print)
    }
    trainModel("test1.dat", "test1Model.txt")
  }
}