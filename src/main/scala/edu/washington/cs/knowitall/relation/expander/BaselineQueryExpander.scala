package edu.washington.cs.knowitall.relation.expander

import edu.washington.cs.knowitall.relation.experiment.BenchmarkQuery
import edu.washington.cs.knowitall.model.OpenIeQuery
import edu.washington.cs.knowitall.model.QueryRel
import edu.washington.cs.knowitall.model.QueryArg

object BaselineQueryExpander extends QueryExpander {
  override def getName() = "Baseline"
    
  override def expandQuery(rawQuery: BenchmarkQuery): OpenIeQuery = {
    new OpenIeQuery(
      QueryArg.fromString(rawQuery.arg1.getOrElse("")),
      QueryRel.fromString(rawQuery.rel.getOrElse("")),
      QueryArg.fromString(rawQuery.arg2.getOrElse(""))
    )
  }
}