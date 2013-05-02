package edu.washington.cs.knowitall.model

import edu.washington.cs.knowitall.relation.PhraseNormalizer

case class QueryRel(
    rel: Option[String]=None,
    srlLinks: Option[Set[String]]=None,
    wnLinks: Option[Set[String]]=None,
    vnLinks: Option[Set[String]]=None) {
  def getRelQueryString(): Option[String] = {
    rel match {
      case Some(str) => Some("\"%s\"".format(PhraseNormalizer.normalize(str)))
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
        new QueryRel(rel=Some(str))
      }
    }
  }
}