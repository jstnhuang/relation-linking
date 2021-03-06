package edu.washington.cs.knowitall.model

import edu.washington.cs.knowitall.relation.PhraseNormalizer

case class QueryRel(
    rels: Option[Set[String]]=None,
    srlLinks: Option[Set[String]]=None,
    wnLinks: Option[Set[String]]=None,
    vnLinks: Option[Set[String]]=None) {
  
  def getFirstRel(): Option[String] = {
    rels match {
      case Some(phrases) => if (phrases.isEmpty) { None } else { Some(phrases.toList(0)) }
      case None => None
    }
  }
  
  def getRelQueryString(): Option[String] = {
    rels match {
      case Some(phrases) => Some(phrases.map({ phrase =>
        "\"%s\"".format(PhraseNormalizer.normalize(phrase))
      }).mkString(" OR "))
      case None => None
    }
  }
  def setOptionToUnionString(set: Option[Set[String]]): Option[String] = {
    set match {
      case Some(links) => Some(links.map("\"%s\"".format(_)).mkString(" OR "))
      case None => None
    }
  }
  def getSrlQueryString(): Option[String] = setOptionToUnionString(srlLinks)
  def getWnQueryString(): Option[String] = setOptionToUnionString(wnLinks)
  def getVnQueryString(): Option[String] = setOptionToUnionString(vnLinks)
  
  override def toString(): String = {
    val relString = rels match {
      case Some(phrases) => "Strings: " + phrases.mkString(", ")
      case None => None
    }
    val srlString = srlLinks match {
      case Some(links) => "SRL: " + links.mkString(", ")
      case None => ""
    }
    val wnString = wnLinks match {
      case Some(links) => "WordNet: " + links.mkString(", ")
      case None => ""
    }
    val vnString = vnLinks match {
      case Some(links) => "VerbNet: " + links.mkString(", ")
      case None => ""
    }
    List(relString, srlString, wnString, vnString).filterNot(_ == "").mkString("; ")
  }
}

object QueryRel {
  /**
   * Assumes only one link is passed in.
   */
  def fromString(str: String): QueryRel = {
    if (str.startsWith("srl:")) {
      new QueryRel(srlLinks=Some(Set(str.substring(4))))
    } else if (str.startsWith("wn:")) {
      new QueryRel(wnLinks=Some(Set(str.substring(3))))
    } else if (str.startsWith("vn:")) {
      new QueryRel(vnLinks=Some(Set(str.substring(3))))
    } else {
      if (str == "") {
        new QueryRel()
      } else {
        new QueryRel(rels=Some(Set(str)))
      }
    }
  }
}