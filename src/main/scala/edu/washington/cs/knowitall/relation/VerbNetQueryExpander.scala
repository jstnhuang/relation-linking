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
  val verbNetLinker = new VerbNetRelationLinker(
    derbyHandler,
    Constants.RELATION_BASEPATH + Constants.WORDNET_DICT
  )
  
  override def getName(): String = "VerbNet"
    
  override def expandQuery(rawQuery: BenchmarkQuery): OpenIeQuery = {
    val queryArg1 = QueryArg.fromString(rawQuery.arg1.getOrElse(""))
    val queryRel = QueryRel.fromString(rawQuery.rel.getOrElse(""))
    val queryArg2 = QueryArg.fromString(rawQuery.arg2.getOrElse(""))
    val (arg1Tags, relTags, arg2Tags) = tagQuery(queryArg1, queryRel, queryArg2)
    
    // Find VerbNet senses for this query.
    val verbNetSenses = verbNetLinker.getRelationLinks(relTags)
    
    if (verbNetSenses.size == 0) {
      System.err.println("No entailed VerbNet senses for " + queryRel.rel.getOrElse("(None)"))
      new OpenIeQuery(
        QueryArg.fromString(rawQuery.arg1.getOrElse("")),
        new QueryRel(),
        QueryArg.fromString(rawQuery.arg2.getOrElse(""))
      )
    } else {
      // Find all entailed VerbNet senses.
      val queryString = (
        "SELECT vn2 FROM vn_to_vn WHERE vn1 IN ("
        + verbNetSenses.toSeq.map(_ => "?").mkString(", ") + ")"
      )
      val selectStatement = derbyHandler.prepareStatement(queryString)
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
}