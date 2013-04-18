package edu.washington.cs.knowitall

import edu.mit.jwi.IDictionary
import edu.mit.jwi.Dictionary
import java.io.File
import edu.mit.jwi.item.IWord
import edu.mit.jwi.item.POS

class WordNetUtils(wordNetPath: String) {
  val wordNet = loadFromPath(wordNetPath)
  
  def loadFromPath(path: String): IDictionary = {
    val f = new File(path)
    val wordNet = new Dictionary(new File(path))
    wordNet.open()
    wordNet
  }
  
  def getWordSense(word: String, partOfSpeech: POS, senseNumber: Integer): IWord = {
    wordNet.getWord(wordNet.getIndexWord(word, partOfSpeech).getWordIDs().get(senseNumber-1))
  }
}