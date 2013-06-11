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
   * Gets the most frequent WordNet sense of the phrase. If the phrase is not found in WordNet, chop
   * off the last word and try again. Returns the empty set if no WordNet senses found at all. If
   * there is a WordNet sense, it will return the longest one.
   */
  def getWordRelationLinks(
      phrase: Seq[PostaggedToken],
      context: Option[(Seq[PostaggedToken], Interval)] = None,
      maxNumSenses: Integer = 3): Option[(Seq[String], Set[IWord], Seq[String])] = {
    val (headPhrase, headIndex) = RelationPhraseFinder.getHeadPhrase(phrase)
    val words = phrase.map(_.string).map(PhraseNormalizer.normalize(_))
    val preHeadWords = words.take(headIndex)
    val headWords = words.drop(headIndex)
    
    /**
     * Check if the given subphrase exists as a verb in WordNet. If so, return it. Otherwise, chop
     * off the rightmost word and search again.
     */
    def checkSubstring(subphrase: Seq[String]): Option[Seq[String]] = {
      if (subphrase.size == 0) {
        None
      } else {
        val currentPhrase = subphrase.mkString(" ")
        val wordSenses = wordNetUtils.getWordSenses(currentPhrase, POS.VERB);
        if (wordSenses != null) {
          Some(subphrase)
        } else {
          checkSubstring(subphrase.dropRight(1))
        }
      }
    }
    
    val subphraseOption = checkSubstring(headWords)
    subphraseOption match {
      case Some(subphrase) => {
        val currentPhrase = subphrase.mkString(" ")
        val wordSenses = wordNetUtils.getWordSenses(currentPhrase, POS.VERB);
        val wordNetSenses = wordSenses.take(maxNumSenses).toSet
        val postHeadWords = words.drop(headIndex + subphrase.size)
        Some((preHeadWords, wordNetSenses, postHeadWords))
      }
      case None => None
    }
  }
  
  /**
   * Same as getWordRelationLinks, but returns Strings instead of IWords.
   */
  def getRelationLinks(
      phrase: Seq[PostaggedToken],
      context: Option[(Seq[PostaggedToken], Interval)] = None): Set[String] = {
    getWordRelationLinks(phrase, context) match {
      case Some((preHeadWords, wordNetSenses, postHeadWords)) => {
        wordNetSenses.map(wordNetUtils.wordToString(_))
      }
      case None => Set.empty[String]
    }
  }
}