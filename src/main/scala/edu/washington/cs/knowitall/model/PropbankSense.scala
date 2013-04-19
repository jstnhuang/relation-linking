package edu.washington.cs.knowitall.model

import scala.io.Source

case class PropbankSense(verb: String, senseNum: Integer, gloss: String,
    preps: Option[List[String]] = None) {
  def getVerb = verb
  def getSenseNum = senseNum
  def getGloss = gloss
  def getPreps = preps // Prepositions attached to the verb
  
  def getPbSense() = "%s.%02d".format(verb, senseNum)
  
  override def toString() = {
    preps match {
      case Some(prepList) => (getPbSense :: prepList).mkString("_")
      case None => getPbSense
    }
  }
}

object PropbankSense {
  def fromString(string: String, gloss: String = "Unknown gloss",
      preps: Option[List[String]] = None): PropbankSense = {
    val split = "\\.".r.split(string)
    new PropbankSense(split(0), split(1).toInt, gloss, preps)
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