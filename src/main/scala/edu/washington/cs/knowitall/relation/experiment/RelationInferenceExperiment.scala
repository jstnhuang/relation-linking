package edu.washington.cs.knowitall.relation.experiment

import java.io.BufferedWriter

import scala.io.Source

import edu.knowitall.openie.models.{ExtractionGroup, ReVerbExtraction}
import edu.washington.cs.knowitall.SolrQueryExecutor
import edu.washington.cs.knowitall.model.OpenIeQuery
import edu.washington.cs.knowitall.relation.expander.{BaselineQueryExpander, QueryExpander, SrlQueryExpander, VerbNetQueryExpander}
import scopt.OptionParser

class RelationInferenceExperiment (solrUrl: String, inputDir: String, outputDir: String) {
  val solrExecutor = new SolrQueryExecutor(solrUrl)
  val BENCHMARK_QUERIES_FILE = "benchmark-queries.tsv"
  val QUERY_STATS_FILE = "query_stats.txt"
  val SENTENCES_FILE_BASE = "sentences"
  type REG = ExtractionGroup[ReVerbExtraction];
    
  def getTestQueries(): Seq[BenchmarkQuery] = {
    var testQueries = List[BenchmarkQuery]()
    Source.fromFile(inputDir + BENCHMARK_QUERIES_FILE).getLines().foreach({ line =>
      testQueries = BenchmarkQuery.fromLine(line)::testQueries
    })
    testQueries
  }
  
  def runQuery(query: OpenIeQuery): Set[REG] = {
    val queryText = query.getQueryString()
    println(queryText)
    null
//    solrExecutor.execute(queryText).toSet
  }
  
//  def outputStats(writer: BufferedWriter, name: String, query: OpenIeQuery, groups: Set[REG]):
//      Unit = {
//    writer.write(query.getQueryString(false))
//    writer.newLine()
//    writer.write(query.getQueryString(true))
//    writer.newLine()
//    
//    writer.write("Without linking: %d groups, %d sentences.".format(
//      groupsWithoutRlinking.size,
//      groupsWithoutRlinking.foldLeft(0)((sum, group) => sum + group.instances.size)
//    ))
//    writer.newLine()
//    writer.write("   With linking: %d groups, %d sentences.".format(
//      groupsWithRlinking.size,
//      groupsWithRlinking.foldLeft(0)((sum, group) => sum + group.instances.size)
//    ))
//    writer.newLine()
//    writer.newLine()
//  }
  
  /**
   * Output lines of the form: query, result tuple, result sentence, tag
   */
  def outputSentences(writer: BufferedWriter, name: String, query: OpenIeQuery, groups: Set[REG]):
      Unit = {
    groups.foreach({ group =>
      val queryString = query.getQueryString()
      val tag = ""
      writer.write("%s\t%s, %s, %s\t%s".format(
        queryString, group.arg1.norm, group.rel.norm, group.arg2.norm, tag
      ))
      writer.newLine()
//      group.instances.take(5).foreach({instance =>
//        val sentenceText = instance.extraction.sentenceText
//        writer.write("      " + sentenceText)
//        writer.newLine()
//      })
    })
  }
  
  def run(): Unit = {
    val queryExpanders: Seq[QueryExpander] = List(BaselineQueryExpander, SrlQueryExpander, VerbNetQueryExpander)
    val benchmarkQueries = getTestQueries()
    
    queryExpanders.foreach({ expander =>
      val systemName = expander.getName()
      println(systemName)
//      val sentenceWriter = new BufferedWriter(new FileWriter(
//        outputDir + SENTENCES_FILE_BASE + "_" + systemName + ".txt"
//      ))
      benchmarkQueries.foreach({ benchmarkQuery =>
        val query = expander.expandQuery(benchmarkQuery)
        val groups = runQuery(query)
        
//        outputSentences(sentenceWriter, systemName, query, groups)
      })
//      sentenceWriter.close()
    })
  }
}

object RelationInferenceExperiment {
  def main(args: Array[String]): Unit = {
    var solrUrl = ""
    var inputDir = "."
    var outputDir = "."
    
    val parser = new OptionParser() {
      arg("solrUrl", "URL of Solr instance to query.", {str => solrUrl = str})
      arg("inputDir", "Directory of other input files.", {str => inputDir = str})
      arg("outputDir", "Directory to put output files.", {str => outputDir = str})
    }
    
    if (!parser.parse(args)) {
      return
    }
    
    val experiment = new RelationInferenceExperiment(solrUrl, inputDir, outputDir)
    experiment.run()
  }
}
