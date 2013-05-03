package edu.washington.cs.knowitall.relation.linker

import edu.knowitall.tool.postag.PostaggedToken
import edu.knowitall.tool.postag.Postagger
import edu.mit.jwi.IDictionary
import edu.mit.jwi.Dictionary
import java.io.File
import edu.mit.jwi.item.POS
import edu.washington.cs.knowitall.WordNetUtils
import edu.washington.cs.knowitall.relation.RelationPhraseFinder
import edu.washington.cs.knowitall.relation.PhraseNormalizer
import edu.washington.cs.knowitall.db.DerbyHandler

/**
 * Links verb phrases to WordNet senses. Currently just links to the most frequent WordNet sense of
 * a phrase/word.
 */
class WordNetRelationLinker(derbyHandler: DerbyHandler, wordNetPath: String) extends RelationLinker {
  val wordNetUtils = new WordNetUtils(wordNetPath)
  
  /**
   * Gets the most frequent WordNet sense of the phrase. If the phrase is not found in WordNet, chop
   * off the last word and try again. Returns the empty set if no WordNet senses found at all. If
   * there is a WordNet sense, it will return the longest one.
   */
  def getRelationLinks(phrase: Seq[PostaggedToken], context: Option[Seq[PostaggedToken]] = None):
      Set[String] = {
    val headPhrase = RelationPhraseFinder.getHeadPhrase(phrase);
    var words = headPhrase.map(_.string).map(word => PhraseNormalizer.normalize(word))
    var wordNetSenses = Set[String]()
    while(words.size > 0 && wordNetSenses.size == 0) {
      var currentPhrase = words.mkString(" ")
      val word = wordNetUtils.getWordSense(currentPhrase, POS.VERB, 1);
      if (word != null) {
        wordNetSenses += wordNetUtils.wordToString(word)
      }
      words = words.dropRight(1)
    }
    wordNetSenses
  }
}