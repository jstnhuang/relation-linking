/*package edu.washington.cs.knowitall.model

import edu.knowitall.tool.tokenize.OpenNlpTokenizer
import edu.knowitall.tool.stem.MorphaStemmer

class QueryPart {
  val tokenizer = new OpenNlpTokenizer()
  def normalizeString(str: String): String = {
    val tokenized = tokenizer.synchronized { tokenizer.tokenize(str) }
    val lemmatized = tokenized.map(_.string) map MorphaStemmer.lemmatize
    lemmatized.mkString(" ")
  }
}*/