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

class WordNetUtils(wordNetPath: String) {
  val wordNet = loadFromPath(wordNetPath)
  
  def loadFromPath(path: String): IDictionary = {
    val f = new File(path)
    val wordNet = new Dictionary(new File(path))
    wordNet.open()
    wordNet
  }
  
  def getWordSense(word: String, partOfSpeech: POS, senseNumber: Integer): IWord = {
    try {
      val indexWord = wordNet.getIndexWord(word, partOfSpeech)
      val wordIds = indexWord.getWordIDs()
      val wordId = wordIds.get(senseNumber-1)
      wordNet.getWord(wordId)
    } catch {
      case e: Exception => System.err.println("Error: couldn't get word sense for " + word); null
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
}