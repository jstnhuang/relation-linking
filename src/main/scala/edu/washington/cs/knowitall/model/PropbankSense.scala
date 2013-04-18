package edu.washington.cs.knowitall.model

case class PropbankSense(verb: String, senseNum: Integer) {
  override def toString() = {
    "%s.%02d".format(verb, senseNum)
  }
}

object PropbankSense {
  def fromString(string: String): PropbankSense = {
    val split = "\\.".r.split(string)
    PropbankSense(split(0), split(1).toInt)
  }
}