package edu.washington.cs.knowitall.relation

import edu.washington.cs.knowitall.relation.experiment.BenchmarkQuery
import edu.washington.cs.knowitall.model.OpenIeQuery
import edu.knowitall.tool.tokenize.OpenNlpTokenizer
import edu.washington.cs.knowitall.model.QueryRel
import edu.washington.cs.knowitall.model.QueryArg
import scala.collection.JavaConverters._
import edu.washington.cs.knowitall.relation.linker.VerbNetRelationLinker
import edu.washington.cs.knowitall.db.DerbyHandler

object VerbNetQueryExpander extends QueryExpander {
  val derbyHandler = new DerbyHandler(Constants.RELATION_BASEPATH + Constants.VNTABLES)
  
  override def getName(): String = "VerbNet"
    
  override def expandQuery(rawQuery: BenchmarkQuery): OpenIeQuery = {
    val queryArg1 = QueryArg.fromString(rawQuery.arg1.getOrElse(""))
    val queryRel = QueryRel.fromString(rawQuery.rel.getOrElse(""))
    val queryArg2 = QueryArg.fromString(rawQuery.arg2.getOrElse(""))
    val (arg1Tags, relTags, arg2Tags) = tagQuery(queryArg1, queryRel, queryArg2)
    
    // Find VerbNet senses for this query.
    val verbNetLinker = new VerbNetRelationLinker(Constants.RELATION_BASEPATH)
    val verbNetSenses = verbNetLinker.getRelationLinks(relTags)
    
    // Find all entailed VerbNet senses.
    val selectStatement = derbyHandler.prepareStatement(
      "SELECT vn2 FROM vn_to_vn WHERE vn1 IN ("
      + verbNetSenses.map(_ => "?").mkString(", ") + ")"
    )
    var index=1;
    verbNetSenses.foreach({ verbNetSense =>
      selectStatement.setString(index, verbNetSense)
      index += 1
    })
    
    val results = derbyHandler.query(selectStatement)
    var entailedSenses = Set[String]()
    while(results.next()) {
      val entailedSense = results.getString(1)
      entailedSenses += entailedSense
    }
    
    new OpenIeQuery(
      QueryArg.fromString(rawQuery.arg1.getOrElse("")),
      new QueryRel(vnLinks=Some(entailedSenses)),
      QueryArg.fromString(rawQuery.arg2.getOrElse(""))
    )
  }
}