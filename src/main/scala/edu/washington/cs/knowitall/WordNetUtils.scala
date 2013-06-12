package edu.washington.cs.knowitall

import edu.mit.jwi.IDictionary
import edu.mit.jwi.Dictionary
import java.io.File
import edu.mit.jwi.item.IWord
import edu.mit.jwi.item.POS
import scala.collection.JavaConverters._
import edu.mit.jwi.item.Pointer
import edu.mit.jwi.item.IIndexWord
import edu.mit.jwi.item.IWordID

class WordNetUtils(wordNetPath: File) {
  val wordNet = loadFromPath(wordNetPath)
  
  def loadFromPath(path: File): IDictionary = {
    val wordNet = new Dictionary(path)
    wordNet.open()
    wordNet
  }
  
  def getWordSense(word: String, partOfSpeech: POS, senseNumber: Integer): IWord = {
    try {
      val indexWord = wordNet.getIndexWord(word, partOfSpeech)
      val wordIds = indexWord.getWordIDs()
      val wordId = wordIds.get(senseNumber-1)
      val result = wordNet.getWord(wordId)
      result
    } catch {
      case e: Exception => {
//        System.err.println("Error: couldn't get word sense for \"%s\"".format(word));
        null
      }
    }
  }
  
  def getWordSenses(word: String, partOfSpeech: POS): Seq[IWord] = {
    try {
      val indexWord = wordNet.getIndexWord(word, partOfSpeech)
      val wordIds = indexWord.getWordIDs().asScala
      wordIds.map(wordNet.getWord(_))
    } catch {
      case e: Exception => {
//        System.err.println("Error: couldn't get word sense for \"%s\"".format(word));
        null
      }
    }
  }
  
  def getSynonyms(word: IWord): Set[IWord] = {
    word.getSynset().getWords().asScala.toSet
  }
  
  def getHyponyms(word: IWord): Set[IWord] = {
    val relatedSynsetIds = word.getSynset().getRelatedSynsets(Pointer.HYPONYM).asScala
    val synsets = relatedSynsetIds.map({ synsetId => wordNet.getSynset(synsetId) })
    synsets.flatMap({synset => synset.getWords().asScala}).toSet
  }
  
  def getHypernyms(word: IWord): Set[IWord] = {
    val relatedSynsetIds = word.getSynset().getRelatedSynsets(Pointer.HYPERNYM).asScala
    val synsets = relatedSynsetIds.map({ synsetId => wordNet.getSynset(synsetId) })
    synsets.flatMap({synset => synset.getWords().asScala}).toSet
  }
  
  def getTagCount(word: IWord): Integer = {
    wordNet.getSenseEntry(word.getSenseKey()).getTagCount()
  }
  
  def getSenseNumber(word: IWord): Integer = {
    wordNet.getSenseEntry(word.getSenseKey()).getSenseNumber()
  }
  
  def getGloss(word: IWord): String = {
    word.getSynset().getGloss()
  }
  
  def wordToString(word: IWord, senseNumber: Boolean = true, tagCount: Boolean = false): String = {
    var result = word.getLemma()
    if (senseNumber) {
      result = "%s#%d".format(result, getSenseNumber(word))
    }
    if (tagCount) {
      result = "%s (%d)".format(result, getTagCount(word))
    }
    result
  }
  
  /**
   * Assumes a string of the form take_a_breather#1.
   */
  def wordFromString(str: String): IWord = {
    val parts = "#".r.split(str).map(_.trim)
    val lemma = parts(0).replace("_", " ")
    val senseNumber = parts(1).toInt
    getWordSense(lemma, POS.VERB, senseNumber)
  }
}