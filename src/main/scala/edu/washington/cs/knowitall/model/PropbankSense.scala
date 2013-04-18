package edu.washington.cs.knowitall.model

import scala.io.Source

case class PropbankSense(verb: String, senseNum: Integer, gloss: String) {
  def getVerb = verb
  def getSenseNum = senseNum
  def getGloss = gloss
  
  override def toString() = {
    "%s.%02d".format(verb, senseNum)
  }
}

object PropbankSense {
  def fromString(string: String, gloss: String = "Unknown gloss"): PropbankSense = {
    val split = "\\.".r.split(string)
    new PropbankSense(split(0), split(1).toInt, gloss)
  }
}

class PropbankGlossReader(glossPath: String) {
  val glosses = readGlosses(glossPath)
  def readGlosses(glossPath: String): Map[String, String] = {
    Source.fromFile(glossPath).getLines.map({ line =>
      val split = "\t".r.split(line)
      split(0) -> split(1)
    }).toMap
  }
}