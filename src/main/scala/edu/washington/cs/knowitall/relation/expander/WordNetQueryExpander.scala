package edu.washington.cs.knowitall.relation.expander

import edu.washington.cs.knowitall.relation.RelationPhraseFinder
import edu.washington.cs.knowitall.relation.experiment.BenchmarkQuery
import edu.washington.cs.knowitall.model.OpenIeQuery
import edu.washington.cs.knowitall.WordNetUtils
import edu.washington.cs.knowitall.relation.linker.WordNetRelationLinker
import edu.washington.cs.knowitall.model.QueryRel
import edu.washington.cs.knowitall.model.QueryArg

/**
 * Expands the relation phrase of a query based on its WordNet sense.
 */
class WordNetQueryExpander(wordNetPath: String) extends QueryExpander {
  val wordNetUtils = new WordNetUtils(wordNetPath)
  val wordNetLinker = new WordNetRelationLinker(wordNetPath)
  
  def getName(): String = "WordNet"

  def expandQuery(rawQuery: BenchmarkQuery): OpenIeQuery = {
    val queryArg1 = QueryArg.fromString(rawQuery.arg1.getOrElse(""))
    val queryRel = QueryRel.fromString(rawQuery.rel.getOrElse(""))
    val queryArg2 = QueryArg.fromString(rawQuery.arg2.getOrElse(""))
    val (arg1Tags, relTags, arg2Tags) = tagQuery(queryArg1, queryRel, queryArg2)
    
    val wordNetSenses = wordNetLinker.getWordRelationLinks(relTags)
    val preps = RelationPhraseFinder.getPrepositions(relTags)
    val relPreps = if(!preps.isEmpty) {
      Some(preps.map(_.string).mkString(" "))
    } else {
      None
    }
    
    if (wordNetSenses.size == 0) {
      System.err.println("No entailed WordNet senses for " + queryRel.rel.getOrElse("(None)"))
      null
    } else {
      val entailedSenses = wordNetSenses.flatMap { sense =>
        val synonyms = wordNetUtils.getSynonyms(sense)
        val hyponyms = wordNetUtils.getHyponyms(sense)
        synonyms ++ hyponyms
      }.map(wordNetUtils.wordToString(_))
      new OpenIeQuery(
        queryArg1,
        new QueryRel(rel=relPreps, wnLinks=Some(entailedSenses)),
        queryArg2
      )
    }
  }
  
  def getWordNetLinker(): WordNetRelationLinker = {
    wordNetLinker
  }
}
