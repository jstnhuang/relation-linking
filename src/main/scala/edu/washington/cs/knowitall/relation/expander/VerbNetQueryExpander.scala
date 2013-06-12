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
import edu.washington.cs.knowitall.WordNetUtils
import edu.washington.cs.knowitall.relation.linker.EntailmentDirection._

/**
 * Expands the relation phrase of a query based on its VerbNet sense.
 */
class VerbNetQueryExpander(verbNetDbPath: String, wordNetUtils: WordNetUtils)
    extends QueryExpander {
  val verbNetLinker = new VerbNetRelationLinker(verbNetDbPath, wordNetUtils, Troponym)
  val derbyHandler = new DerbyHandler(verbNetDbPath)
  
  override def getName(): String = "VerbNet"
    
  override def expandQuery(rawQuery: BenchmarkQuery): OpenIeQuery = {
    val queryArg1 = QueryArg.fromString(rawQuery.arg1.getOrElse(""))
    val queryRel = QueryRel.fromString(rawQuery.rel.getOrElse(""))
    val queryArg2 = QueryArg.fromString(rawQuery.arg2.getOrElse(""))
    val relString = queryRel.getFirstRel.get
    val (arg1Tags, relTags, arg2Tags) = QueryExpander.tagQuery(queryArg1, relString, queryArg2)
    
    // Find VerbNet senses for this query.
    verbNetLinker.getRelationLinks(relTags) match {
      case Some((preHeadWords, verbNetSenses, postHeadWords)) => {
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
        var entailingSenses = Set[String]()
        while(results.next()) {
          val entailingSense = results.getString(1)
          entailingSenses += entailingSense
        }
        
        val rels = entailingSenses.map({ sense =>
          val verbNetLemma = parseVerbNetSense(sense)
          ((preHeadWords :+ verbNetLemma) ++ postHeadWords).mkString(" ")
        })
        
        new OpenIeQuery(
          queryArg1,
          new QueryRel(rels=Some(rels)),
          queryArg2
        )
      }
      case None => {
        System.err.println(
          "No entailed VerbNet senses for " + queryRel.getFirstRel.getOrElse("(None)")
        )
        null
      }
    }
  }
  
  def parseVerbNetSense(verbNetSense: String): String = {
    "_".r.split(verbNetSense).flatMap({ word =>
      "-".r.split(word).take(1)
    }).mkString(" ")
  }
}
