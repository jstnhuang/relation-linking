package edu.washington.cs.knowitall.model

import edu.washington.cs.knowitall.relation.HtmlGroupingRelationLinker
import scala.collection.JavaConverters._
import edu.knowitall.tool.tokenize.OpenNlpTokenizer
import edu.knowitall.tool.stem.MorphaStemmer

/**
 * A not fully featured class representing an Open IE query.
 */
class OpenIeQuery (
    arg1: Option[String] = None,
    rel: Option[String] = None,
    arg2: Option[String] = None,
    arg1Type: Option[String] = None,
    arg2Type: Option[String] = None,
    arg1_entity: Option[String] = None,
    arg2_entity: Option[String] = None) {
  val relationLinks = rel match {
    case Some(relString) => {
      val normalizedString = OpenIeQuery.normalize(relString)
      Some(OpenIeQuery.relationLinker.getRelationLinks(normalizedString).asScala)
    }
    case None => None
  }
  
  /**
   * Makes a Solr query string based on the fields of the query.
   */
  def getQueryString(rlinking: Boolean = false): String = {
    val argNames = List("arg1", "arg2")
    val argValues = List(
      OpenIeQuery.getNormalizedArgString(arg1),
      OpenIeQuery.getNormalizedArgString(arg2)
    )
    val argQuery = (argNames zip argValues).map({ tuple =>
      tuple match {
        case (name, Some(part)) => Some("+%s: (%s)".format(name, part))
        case _ => None
      }
    }).flatMap(x => x).mkString(" ")
    
    val otherNames = List("arg1_types", "arg2_types", "arg1_entity_id", "arg2_entity_id")
    val otherValues = List(arg1Type, arg2Type, arg1_entity, arg2_entity)
    val otherQuery = (otherNames zip otherValues).map({ tuple =>
      tuple match {
        case (name, Some(part)) => Some("+%s: (\"%s\")".format(name, part))
        case _ => None
      }
    }).flatMap(x => x).mkString(" ")
    
    val relQuery = rel match {
      case Some(relString) => {
        val normalizedString = OpenIeQuery.normalize(relString)
        val rlinkString = if(rlinking) {
          if (relationLinks.isEmpty || relationLinks.get.isEmpty) {
            ""
          } else {
            val allLinksString = relationLinks.get.map(x => "\"%s\"".format(x)).mkString(" OR ")
            " OR rel_link_id: (%s)".format(allLinksString)
          }
          
        } else {
          ""
        }
        "+(rel: (\"%s\")".format(normalizedString) + rlinkString + ")"
      }
      case _ => ""
    }
    List(argQuery, otherQuery, relQuery).mkString(" ").trim()
  }
}

object OpenIeQuery {
  val relationLinker = new HtmlGroupingRelationLinker("/scratch2/rlinking/")
  val tokenizer = new OpenNlpTokenizer()
  
  def getNormalizedArgString(string: Option[String]): Option[String] = {
    string match {
      case Some(str) => Some(getQueryTerms(str).map({x => "\"%s\"".format(x)}).mkString(" OR "))
      case None => None
    }
  }
  
  def getQueryTerms(term: String): Set[String] = {
    val normalizedTerm = normalize(term)
    if (normalizedTerm != term) {
      Set(normalizedTerm, term)
    } else {
      Set(term)
    }
  }
  
  def normalize(terms: String): String = {
    val tokenized = tokenizer.synchronized {
      tokenizer.tokenize(terms)
    }
    (tokenized.map(_.string) map MorphaStemmer.lemmatize).mkString(" ")
  }
  
  /**
   * Constructs an OpenIeQuery instance given the raw strings from the query. The args may contain
   * fields like "type:Organization" or "entity:Paris", although this doesn't support entities right
   * now.
   */
  def fromStrings(arg1String: String, relString: String, arg2String : String): OpenIeQuery = {
    var arg1 = if (arg1String == "") { None } else { Some(arg1String) }
    var arg2 = if (arg2String == "") { None } else { Some(arg2String) }
    val arg1Type = if (arg1String.startsWith("type:")) {
      arg1 = None;
      Some(arg1String.substring(5))
    } else { None }
    val arg2Type = if (arg2String.startsWith("type:")) {
      arg2 = None;
      Some(arg2String.substring(5))
    } else { None }
    
    val rel = Some(relString)
    
    new OpenIeQuery(
      arg1=arg1, rel=rel, arg2=arg2,
      arg1Type=arg1Type, arg2Type=arg2Type
    )
  }
}