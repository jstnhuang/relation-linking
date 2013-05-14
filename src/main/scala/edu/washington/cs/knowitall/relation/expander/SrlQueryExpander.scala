package edu.washington.cs.knowitall.relation.expander

import edu.washington.cs.knowitall.relation.RelationPhraseFinder
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
    val relString = queryRel.rels.mkString(" ")
    val (arg1Tags, relTags, arg2Tags) = QueryExpander.tagQuery(queryArg1, relString, queryArg2)
    val sentence = arg1Tags ++ relTags ++ arg2Tags
    val relInterval = Interval.span(relTags.map(_.interval))
    val srlLinks = SrlRelationLinker.getRelationLinks(relTags, Some((sentence, relInterval)))
    val preps = RelationPhraseFinder.getPrepositions(relTags)
    val relPreps = if (!preps.isEmpty) {
      Some(Set(preps.map(_.string).mkString(" ")))
    } else {
      None
    }
    if (srlLinks.size == 0) {
      System.err.println("No SRL senses for " + queryRel.rels.getOrElse("(None)"))
      null
    } else {
      new OpenIeQuery(
        queryArg1,
        new QueryRel(rels=relPreps, srlLinks=Some(srlLinks)),
        queryArg2
      )
    }
  }
}
