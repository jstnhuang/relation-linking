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
class WordNetQueryExpander(wordNetUtils: WordNetUtils) extends QueryExpander {
  val wordNetLinker = new WordNetRelationLinker(wordNetUtils)
  val MAX_NUM_SENSES = 3
  
  def getName(): String = "WordNet"

  def expandQuery(rawQuery: BenchmarkQuery): OpenIeQuery = {
    val queryArg1 = QueryArg.fromString(rawQuery.arg1.getOrElse(""))
    val queryRel = QueryRel.fromString(rawQuery.rel.getOrElse(""))
    val queryArg2 = QueryArg.fromString(rawQuery.arg2.getOrElse(""))
    val relString = queryRel.getFirstRel.get
    val (arg1Tags, relTags, arg2Tags) = QueryExpander.tagQuery(queryArg1, relString, queryArg2)
    
    val wordNetSenses = wordNetLinker.getWordRelationLinks(relTags, maxNumSenses=MAX_NUM_SENSES)
    val preps = RelationPhraseFinder.getPrepositions(relTags)
    val rels = if(!preps.isEmpty) {
      Some(Set(preps.map(_.string).mkString(" ")))
    } else {
      None
    }
    
    if (wordNetSenses.isEmpty) {
      System.err.println("No WordNet senses for " + queryRel.getFirstRel.getOrElse("(None)"))
      null
    } else {
      val entailingSenses = wordNetSenses.flatMap { sense =>
        val synonyms = wordNetUtils.getSynonyms(sense)
        val hyponyms = wordNetUtils.getHyponyms(sense)
        synonyms ++ hyponyms
      }.filter(wordNetUtils.getSenseNumber(_) <= MAX_NUM_SENSES).map({ sense => 
        "%s#%d".format(sense.getLemma(), wordNetUtils.getSenseNumber(sense))
      })
      new OpenIeQuery(
        queryArg1,
        new QueryRel(rels=rels, wnLinks=Some(entailingSenses)),
        queryArg2
      )
    }
  }
}
