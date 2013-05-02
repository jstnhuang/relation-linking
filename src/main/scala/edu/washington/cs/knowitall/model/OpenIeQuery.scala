package edu.washington.cs.knowitall.model

import scala.collection.JavaConverters._
import edu.knowitall.tool.tokenize.OpenNlpTokenizer
import edu.knowitall.tool.stem.MorphaStemmer

/**
 * A not fully featured class representing an Open IE query.
 */
class OpenIeQuery (queryArg1: QueryArg, queryRel: QueryRel, queryArg2: QueryArg) {
  /**
   * Makes a Solr query string based on the fields of the query.
   */
  def getQueryString(): String = {
    val queryParts = List(
      ("arg1", queryArg1.getArgQueryString()),
      ("arg1_entity_id", queryArg1.getEntityQueryString()),
      ("arg1_types", queryArg1.getTypeQueryString()),
      ("rel", queryRel.getRelQueryString()),
      ("srl_link", queryRel.getSrlQueryString()),
      ("wn_link", queryRel.getWnQueryString()),
      ("vn_link", queryRel.getVnQueryString()),
      ("arg2", queryArg2.getArgQueryString()),
      ("arg2_entity_id", queryArg2.getEntityQueryString()),
      ("arg2_types", queryArg2.getTypeQueryString())
    )
    queryParts.map({ queryPart =>
      queryPart match {
        case (name, Some(part)) => Some("+%s: (%s)".format(name, part))
        case _ => None
      }
    }).flatMap(x => x).mkString(" ")
  }
}