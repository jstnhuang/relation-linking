package edu.washington.cs.knowitall.relation.experiment

import java.io.{BufferedWriter, File, FileWriter}
import scala.io.Source
import edu.knowitall.openie.models.{ExtractionGroup, ReVerbExtraction}
import edu.washington.cs.knowitall.SolrQueryExecutor
import edu.washington.cs.knowitall.model.{OpenIeQuery, QueryArg, QueryRel}
import edu.washington.cs.knowitall.relation.Constants
import edu.washington.cs.knowitall.relation.expander.{BaselineQueryExpander, QueryExpander, SrlQueryExpander, VerbNetQueryExpander, WordNetQueryExpander}
import edu.washington.cs.knowitall.relation.linker._
import scopt.OptionParser
import edu.washington.cs.knowitall.WordNetUtils
import edu.washington.cs.knowitall.db.DerbyHandler

class ExpansionTracer(inputDir: String, outputDir: String) {
  val WORDNET_PATH = Constants.wordNetPath(inputDir)
  val VERBNET_PATH = Constants.relationLinkingDbPath(inputDir)
  val BENCHMARK_QUERIES_PATH = List(inputDir, "benchmark-queries.tsv").mkString(File.separator)
  val TRACE_QUERY_EXP_PATH = List(outputDir, "trace-query-exp.tsv").mkString(File.separator)
  val TRACE_LINK_PATH = List(outputDir, "trace-linker.tsv").mkString(File.separator)
  type REG = ExtractionGroup[ReVerbExtraction];
  
  /**
   * Reads test queries from input file.
   */
  def getTestQueries(): Seq[BenchmarkQuery] = {
    var testQueries = List[BenchmarkQuery]()
    Source.fromFile(BENCHMARK_QUERIES_PATH).getLines().foreach({ line =>
      testQueries = BenchmarkQuery.fromLine(line)::testQueries
    })
    testQueries
  }
  
  /**
   * Runs the experiment for a given set of query expanders.
   */
  def run(): Unit = {
    val derbyHandler = new DerbyHandler(VERBNET_PATH)
    val wordNetUtils = new WordNetUtils(WORDNET_PATH)
    val wordNetLinker = new WordNetRelationLinker(wordNetUtils)
    val verbNetExpander = new VerbNetQueryExpander(VERBNET_PATH, wordNetUtils)
    val benchmarkQueries = getTestQueries()
    
    val traceQueryExpWriter = new BufferedWriter(new FileWriter(TRACE_QUERY_EXP_PATH))
    val traceLinkWriter = new BufferedWriter(new FileWriter(TRACE_LINK_PATH))
    benchmarkQueries.foreach({ benchmarkQuery =>
      val queryArg1 = QueryArg.fromString(benchmarkQuery.arg1.getOrElse(""))
      val queryRel = QueryRel.fromString(benchmarkQuery.rel.getOrElse(""))
      val queryArg2 = QueryArg.fromString(benchmarkQuery.arg2.getOrElse(""))
      val relString = queryRel.rels.mkString(" ")
      val (arg1Tags, relTags, arg2Tags) = QueryExpander.tagQuery(queryArg1, relString, queryArg2)
      
      // Link relation phrase to a WordNet sense
      val wordSenses = wordNetLinker.getWordRelationLinks(relTags)
      wordSenses.foreach({ wordSense =>
        // Get synonyms or troponyms of each WordNet sense
        val synonyms = wordNetUtils.getSynonyms(wordSense)
        val troponyms = wordNetUtils.getHyponyms(wordSense)
        val hypernyms = wordNetUtils.getHypernyms(wordSense)
        
        val (querySense, synOrTro) = if (!synonyms.isEmpty) {
          (synonyms, "synonym")
        } else {
          (troponyms, "troponym")
        }
        
        val (tupleSenses, synOrHyp) = if (!synonyms.isEmpty) {
          (synonyms, "synonym")
        } else {
          (hypernyms, "hypernym")
        }
        
        querySense.foreach({ querySense =>
          val wnToVnStatement = derbyHandler.prepareStatement(
            "SELECT vn FROM wn_to_vn WHERE wn=?"
          )
          wnToVnStatement.setString(1, wordNetUtils.wordToString(querySense))
          val wnToVnResults = derbyHandler.query(wnToVnStatement)
          while (wnToVnResults.next()) {
            val verbNetSense = wnToVnResults.getString(1)
            val vnToVnStatement = derbyHandler.prepareStatement(
              "SELECT vn1 FROM vn_to_vn WHERE vn2 = ?"
            )
            vnToVnStatement.setString(1, verbNetSense)
            val vnToVnResults = derbyHandler.query(vnToVnStatement)
            while (vnToVnResults.next()) {
              val entailingVerbNetSense = vnToVnResults.getString(1)
              val line = List(
                queryRel,
                wordNetUtils.wordToString(wordSense, true, true),
                "%s (%s)".format(wordNetUtils.wordToString(querySense, true, true), synOrTro),
                verbNetSense,
                entailingVerbNetSense
              ).mkString("\t")
              traceQueryExpWriter.write(line)
              traceQueryExpWriter.newLine()
            }
          }
        })
        
        tupleSenses.foreach({ tupleSense =>
          val wnToVnStatement = derbyHandler.prepareStatement(
            "SELECT vn FROM wn_to_vn WHERE wn=?"
          )
          wnToVnStatement.setString(1, wordNetUtils.wordToString(tupleSense))
          val wnToVnResults = derbyHandler.query(wnToVnStatement)
          while (wnToVnResults.next()) {
            val verbNetSense = wnToVnResults.getString(1)
            val line = List(
              queryRel,
              wordNetUtils.wordToString(wordSense, true, true),
              "%s (%s)".format(wordNetUtils.wordToString(tupleSense, true, true), synOrHyp),
              verbNetSense
            ).mkString("\t")
            traceLinkWriter.write(line)
            traceLinkWriter.newLine()
          }
        })
      })
    })
    traceQueryExpWriter.close()
    traceLinkWriter.close()
  }
}

object ExpansionTracer {
  def main(args: Array[String]): Unit = {
    var inputDir = "."
    var outputDir = "."
    
    val parser = new OptionParser() {
      arg("inputDir", "Directory of VerbNet tables and WordNet.", {str => inputDir = str})
      arg("outputDir", "Directory to put output files.", {str => outputDir = str})
    }
    
    if (!parser.parse(args)) {
      return
    }
    
    val experiment = new ExpansionTracer(inputDir, outputDir)
    experiment.run()
  }
}
