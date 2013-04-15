package edu.washington.cs.knowitall

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
    case Some(relString) => Some(OpenIeQuery.relationLinker.getRelationLinks(relString).asScala)
    case None => None
  }
  
  /**
   * Makes a Solr query string based on the fields of the query.
   */
  def getQueryString(rlinking: Boolean = false): String = {
    val simpleNames = List("arg1", "arg2", "arg1_types", "arg2_types", "arg1_entity", "arg2_entity")
    val simpleParts = List(arg1, arg2, arg1Type, arg2Type, arg1_entity, arg2_entity)
    val simpleQuery = (simpleNames zip simpleParts).map({ tuple =>
      tuple match {
        case (name, Some(part)) => Some("+%s: (\"%s\")".format(name, part))
        case _ => None
      }
    }).flatMap(x => x).mkString(" ")
    val relQuery = rel match {
      case Some(relString) => {
        val rlinkString = if(rlinking) {
          
          if (relationLinks.isEmpty || relationLinks.get.isEmpty) {
            ""
          } else {
            val allLinksString = relationLinks.map(x => "\"%s\"".format(x)).mkString(" OR ")
            " OR rel_link_id: (%s)".format(allLinksString)
          }
          
        } else {
          ""
        }
        "+(rel: (\"%s\")".format(relString) + rlinkString + ")"
      }
      case _ => ""
    }
    List(simpleQuery, relQuery).mkString(" ")
  }
}

object OpenIeQuery {
  val relationLinker = new HtmlGroupingRelationLinker("/scratch2/rlinking/")
  val tokenizer = new OpenNlpTokenizer()
  
  def normalizeTerms(terms: String): String = {
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
    
    arg1 = arg1 map normalizeTerms
    val rel = Some(normalizeTerms(relString))
    arg2 = arg2 map normalizeTerms
    
    new OpenIeQuery(
      arg1=arg1, rel=rel, arg2=arg2,
      arg1Type=arg1Type, arg2Type=arg2Type
    )
  }
}