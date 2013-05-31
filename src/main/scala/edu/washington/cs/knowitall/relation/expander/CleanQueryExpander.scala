package edu.washington.cs.knowitall.relation.expander

import edu.washington.cs.knowitall.relation.experiment.BenchmarkQuery
import edu.washington.cs.knowitall.model.OpenIeQuery
import edu.washington.cs.knowitall.model.QueryRel
import edu.washington.cs.knowitall.model.QueryArg
import edu.washington.cs.knowitall.db.DerbyHandler
import edu.washington.cs.knowitall.relation.PhraseNormalizer

class CleanQueryExpander(relationLinkDbPath: String) extends QueryExpander {
  val derbyHandler = new DerbyHandler(relationLinkDbPath)

  def getName(): String = "Clean"

  def expandQuery(rawQuery: BenchmarkQuery): OpenIeQuery = {
    val queryArg1 = QueryArg.fromString(rawQuery.arg1.getOrElse(""))
    val queryRel = QueryRel.fromString(rawQuery.rel.getOrElse(""))
    val queryArg2 = QueryArg.fromString(rawQuery.arg2.getOrElse(""))
    val relString = PhraseNormalizer.normalize(queryRel.getFirstRel.get)
    
    val queryString = (
      "SELECT rel1 FROM clean WHERE rel2=?"
    )
    val selectStatement = derbyHandler.prepareStatement(queryString)
    selectStatement.setString(1, relString)
    
    val results = derbyHandler.query(selectStatement)
    var entailingSenses = Set[String]()
    while(results.next()) {
      val entailingSense = results.getString(1)
      entailingSenses += entailingSense
    }
    
    if (entailingSenses.isEmpty) {
      System.err.println("No Clean entailing senses for " + relString);
      null
    } else {
      new OpenIeQuery(
        queryArg1,
        new QueryRel(rels=Some(entailingSenses)),
        queryArg2
      )
    }
  }
}