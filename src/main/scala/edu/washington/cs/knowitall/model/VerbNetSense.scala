package edu.washington.cs.knowitall.model

import scala.io.Source

case class VerbNetSense(verb: String, senseNum: Integer, gloss: String,
    preps: Option[List[String]] = None) {
  def getVerb = verb
  def getSenseNum = senseNum
  def getGloss = gloss
  def getPreps = preps // Prepositions attached to the verb
  
  def getVnSenseString = "%s-%d".format(verb, senseNum)
  
  override def toString() = {
    preps match {
      case Some(prepList) => (getVnSenseString :: prepList).mkString("_")
      case None => getVnSenseString
    }
  }
}