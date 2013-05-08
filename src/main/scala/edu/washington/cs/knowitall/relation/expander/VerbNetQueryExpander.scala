package edu.washington.cs.knowitall.relation.expander

import edu.washington.cs.knowitall.relation.RelationPhraseFinder
import edu.washington.cs.knowitall.relation.experiment.BenchmarkQuery
import edu.washington.cs.knowitall.model.OpenIeQuery
import edu.knowitall.tool.tokenize.OpenNlpTokenizer
import edu.washington.cs.knowitall.model.QueryRel
import edu.washington.cs.knowitall.model.QueryArg
import scala.collection.JavaConverters._
import edu.washington.cs.knowitall.relation.linker.VerbNetRelationLinker
import edu.washington.cs.knowitall.db.DerbyHandler
import edu.washington.cs.knowitall.relation.Constants
import edu.washington.cs.knowitall.relation.linker.WordNetRelationLinker

/**
 * Expands the relation phrase of a query based on its VerbNet sense.
 */
class VerbNetQueryExpander(verbNetDbPath: String, wordNetLinker: WordNetRelationLinker)
    extends QueryExpander {
  val verbNetLinker = new VerbNetRelationLinker(verbNetDbPath, wordNetLinker)
  val derbyHandler = new DerbyHandler(verbNetDbPath)
  
  override def getName(): String = "VerbNet"
    
  override def expandQuery(rawQuery: BenchmarkQuery): OpenIeQuery = {
    val queryArg1 = QueryArg.fromString(rawQuery.arg1.getOrElse(""))
    val queryRel = QueryRel.fromString(rawQuery.rel.getOrElse(""))
    val queryArg2 = QueryArg.fromString(rawQuery.arg2.getOrElse(""))
    val (arg1Tags, relTags, arg2Tags) = tagQuery(queryArg1, queryRel, queryArg2)
    
    // Find VerbNet senses for this query.
    val verbNetSenses = verbNetLinker.getRelationLinks(relTags)
    val preps = RelationPhraseFinder.getPrepositions(relTags)
    val relPreps = if (!preps.isEmpty) {
      Some(relTags.map(_.string).mkString(" "))
    } else {
      None
    }
    
    if (verbNetSenses.size == 0) {
      System.err.println("No entailed VerbNet senses for " + queryRel.rel.getOrElse("(None)"))
      null
    } else {
      // Find all entailing VerbNet senses.
      val queryString = (
        "SELECT vn1 FROM vn_to_vn WHERE vn2 IN ("
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
        queryArg1,
        new QueryRel(rel=relPreps, vnLinks=Some(entailedSenses)),
        queryArg2
      )
    }
  }
}
