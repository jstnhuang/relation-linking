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
import edu.knowitall.collection.immutable.Interval
import edu.mit.jwi.item.Word
import edu.mit.jwi.item.IWord

/**
 * Links verb phrases to WordNet senses. Currently just links to the most frequent WordNet sense of
 * a phrase/word.
 */
class WordNetRelationLinker(wordNetUtils: WordNetUtils)
    extends RelationLinker {
  /**
   * Same as getRelationLinks, but returns IWords instead of Strings.
   */
  def getWordRelationLinks(
      phrase: Seq[PostaggedToken],
      context: Option[(Seq[PostaggedToken], Interval)] = None,
      maxNumSenses: Integer = 3): Set[IWord] = {
    val headPhrase = RelationPhraseFinder.getHeadPhrase(phrase);
    var words = headPhrase.map(_.string).map(word => PhraseNormalizer.normalize(word))
    var wordNetSenses = Set.empty[IWord]
    while(words.size > 0 && wordNetSenses.isEmpty) {
      var currentPhrase = words.mkString(" ")
      val wordSenses = wordNetUtils.getWordSenses(currentPhrase, POS.VERB);
      if (wordSenses != null) {
        wordNetSenses = wordSenses.take(maxNumSenses).toSet
      }
      words = words.dropRight(1)
    }
    wordNetSenses
  }
  
  /**
   * Gets the most frequent WordNet sense of the phrase. If the phrase is not found in WordNet, chop
   * off the last word and try again. Returns the empty set if no WordNet senses found at all. If
   * there is a WordNet sense, it will return the longest one.
   */
  def getRelationLinks(
      phrase: Seq[PostaggedToken],
      context: Option[(Seq[PostaggedToken], Interval)] = None): Set[String] = {
    val wordNetSenses = getWordRelationLinks(phrase, context)
    wordNetSenses.map(wordNetUtils.wordToString(_))
  }
}