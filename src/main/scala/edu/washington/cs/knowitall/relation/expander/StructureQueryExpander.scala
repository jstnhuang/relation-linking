package edu.washington.cs.knowitall.relation.expander

import edu.washington.cs.knowitall.relation.experiment.BenchmarkQuery
import edu.washington.cs.knowitall.model.OpenIeQuery
import edu.washington.cs.knowitall.model.QueryRel
import edu.washington.cs.knowitall.model.QueryArg
import edu.washington.cs.knowitall.relation.PhraseNormalizer
import java.io.File

class StructureQueryExpander(entailmentFile: File) extends QueryExpander {
  def getName(): String = "Structure"
  val entailingSenses = readEntailmentFile(entailmentFile)
  
  def readEntailmentFile(entailmentFile: File): Map[String, Set[String]] = {
    val initialMap = Map.empty[String, Set[String]]
    val tuples = scala.io.Source.fromFile(entailmentFile).getLines().map({ line =>
      val cols = "\t".r.split(line)
      val entailingSense = cols(0)
      val entailedSense = cols(1)
      (entailedSense, entailingSense)
    })
    tuples.foldLeft(initialMap) { case (map, (entailedSense, entailingSense)) =>
      val entailingSenses = map.getOrElse(entailedSense, Set.empty[String])
      map + (entailedSense -> (entailingSenses + entailingSense))
    }
  }

  def expandQuery(rawQuery: BenchmarkQuery): OpenIeQuery = {
    val queryArg1 = QueryArg.fromString(rawQuery.arg1.getOrElse(""))
    val queryRel = QueryRel.fromString(rawQuery.rel.getOrElse(""))
    val queryArg2 = QueryArg.fromString(rawQuery.arg2.getOrElse(""))
    val relString = PhraseNormalizer.normalize(queryRel.getFirstRel.get)
    
    val senses = entailingSenses.getOrElse(relString, Set.empty[String])
    
    if (senses.isEmpty) {
      System.err.println("No structure learning entailing senses for " + relString);
      null
    } else {
      new OpenIeQuery(
        queryArg1,
        new QueryRel(rels=Some(senses + relString)),
        queryArg2
      )
    }
  }
}