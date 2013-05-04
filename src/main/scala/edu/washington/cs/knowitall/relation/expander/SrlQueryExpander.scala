package edu.washington.cs.knowitall.relation.expander

import edu.washington.cs.knowitall.relation.experiment.BenchmarkQuery
import edu.washington.cs.knowitall.model.OpenIeQuery
import edu.washington.cs.knowitall.model.QueryRel
import edu.washington.cs.knowitall.model.QueryArg
import edu.washington.cs.knowitall.relation.linker.SrlRelationLinker
import edu.knowitall.collection.immutable.Interval

/**
 * Expands the relation phrase of a query based on its SRL sense.
 */
object SrlQueryExpander extends QueryExpander {
  def getName(): String = "SRL"

  def expandQuery(rawQuery: BenchmarkQuery): OpenIeQuery = {
    val queryArg1 = QueryArg.fromString(rawQuery.arg1.getOrElse(""))
    val queryRel = QueryRel.fromString(rawQuery.rel.getOrElse(""))
    val queryArg2 = QueryArg.fromString(rawQuery.arg2.getOrElse(""))
    val (arg1Tags, relTags, arg2Tags) = tagQuery(queryArg1, queryRel, queryArg2)
    val sentence = arg1Tags ++ relTags ++ arg2Tags
    val relInterval = Interval.span(relTags.map(_.interval))
    val srlLinks = SrlRelationLinker.getRelationLinks(relTags, Some((sentence, relInterval)))
    if (srlLinks.size == 0) {
      System.err.println("No SRL senses for " + queryRel.rel.getOrElse("(None)"))
      null
    } else {
      new OpenIeQuery(queryArg1,  new QueryRel(srlLinks=Some(srlLinks)), queryArg2)
    }
  }
}