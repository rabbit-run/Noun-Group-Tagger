package edu.nyu.cs.nlp
import edu.nyu.cs.nlp.Util._

class Trainer (fileName: String) {
  
  // compute the features of a specified sentence
  private def featuresOfSentence(previous: Array[String],
      words: List[Array[String]]): List[String] = {
    if (words.tail.isEmpty) Nil
    else computeFeature(previous, words.head, words.tail.head)::featuresOfSentence(words.head, words.tail)
  }

  def getFeatureVectors(): List[String] = {
    val sentences = split_sentences(fileName)
    val start_state = Array("<s>","<s>","<s>")
    val featureVectors =
      for {
        sent <- sentences
        features = this.featuresOfSentence(start_state,sent)
      } yield features
    flatten(featureVectors)
  }
  
  def train(): Unit = {
    val vectors = this.getFeatureVectors()
    val name_stem = this.fileName.split('.')(0)
    printToFile(new java.io.File(name_stem+".dat")){
      p => vectors.foreach(p.print)
    }
    trainModel(name_stem+".dat", name_stem+"Model.txt")
  }
}