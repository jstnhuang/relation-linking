package edu.washington.cs.knowitall.relation

import edu.knowitall.tool.tokenize.OpenNlpTokenizer
import edu.knowitall.tool.stem.MorphaStemmer

object PhraseNormalizer {
  val tokenizer = new OpenNlpTokenizer()
  def normalize(str: String): String = {
    val tokenized = tokenizer.synchronized { tokenizer.tokenize(str) }
    val lemmatized = tokenized.map(_.string) map MorphaStemmer.lemmatize
    lemmatized.mkString(" ")
  }
}