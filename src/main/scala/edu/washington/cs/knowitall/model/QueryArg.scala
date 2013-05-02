package edu.washington.cs.knowitall.model

import edu.knowitall.tool.tokenize.OpenNlpTokenizer
import edu.knowitall.tool.stem.MorphaStemmer
import edu.washington.cs.knowitall.relation.PhraseNormalizer

/**
 * Assumes entity and type are given as strings.
 */
case class QueryArg(
    arg: Option[String]=None,
    entity: Option[String]=None,
    types: Option[String]=None) {
  /**
   * Tokenize and stem the string. If different from original term, OR them. Quote all terms.
   * E.g., "India" -> "indium" OR "India"
   */
  def getArgQueryString(): Option[String] = {
    arg match {
      case Some(arg) => {
        val normalizedArg = PhraseNormalizer.normalize(arg)
        val terms = if (normalizedArg != arg) {
          Set(normalizedArg, arg)
        } else {
          Set(arg)
        }
        
        Some(terms.map("\"%s\"".format(_)).mkString(" OR "))
      }
      case None => None
    }
  }
  
  def getEntityQueryString(): Option[String] = {
    entity match {
      case Some(ent) => Some("\"%s\"".format(ent).replace(" ", "_"))
      case None => None
    }
  }
  
  def getTypeQueryString(): Option[String] = {
    types match {
      case Some(typs) => Some("\"%s\"".format(typs).replace(" ", "_"))
      case None => None
    }
  }
}

object QueryArg {  
  /**
   * Parse a query arg from a string.
   */
  def fromString(str: String): QueryArg = {
    if (str.startsWith("entity:")) {
      new QueryArg(entity=Some(str.substring(7)))
    } else if (str.startsWith("type:")){
      new QueryArg(types=Some(str.substring(5)))
    } else {
      if (str == "") {
        new QueryArg()
      } else {
        new QueryArg(arg=Some(str))
      }
    }
  }
}