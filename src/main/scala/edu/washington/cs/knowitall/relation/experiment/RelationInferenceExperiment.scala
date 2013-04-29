package edu.washington.cs.knowitall.relation.experiment

import java.io.ByteArrayInputStream
import java.io.ObjectInputStream
import scala.Array.canBuildFrom
import scala.collection.JavaConverters.asScalaBufferConverter
import scala.collection.JavaConverters.asScalaIteratorConverter
import scala.io.Source
import org.apache.solr.client.solrj.SolrQuery
import org.apache.solr.client.solrj.impl.HttpSolrServer
import edu.knowitall.common.Resource.using
import edu.washington.cs.knowitall.model.OpenIeQuery
import edu.knowitall.openie.models.ExtractionArgument
import edu.knowitall.openie.models.ExtractionGroup
import edu.knowitall.openie.models.ExtractionRelation
import edu.knowitall.openie.models.FreeBaseEntity
import edu.knowitall.openie.models.FreeBaseType
import edu.knowitall.openie.models.Instance
import edu.knowitall.openie.models.ReVerbExtraction
import scopt.OptionParser
import edu.washington.cs.knowitall.SolrQueryExecutor
import java.io.FileWriter
import java.io.BufferedWriter
import java.io.Writer

class RelationInferenceExperiment (solrUrl: String, inputDir: String, outputDir: String) {
  val solrExecutor = new SolrQueryExecutor(solrUrl)
  val BENCHMARK_QUERIES_FILE = "benchmark-queries.tsv"
  val QUERY_STATS_FILE = "query_stats.txt"
  val SENTENCES_FILE = "sentences.txt"
  
  /**
   * Gets 
   */
  def getTestQueries(): Iterator[OpenIeQuery] = {
    Source.fromFile(inputDir + BENCHMARK_QUERIES_FILE).getLines().map({ line =>
      val columns = line.split("\t").map(_.trim())
      val arg1String = if(columns.length > 0) { columns(0) } else { "" }
      val relString = if(columns.length > 1) { columns(1) } else { "" }
      val arg2String = if(columns.length > 2) { columns(2) } else { "" }
      OpenIeQuery.fromStrings(arg1String, relString, arg2String)
    })
  }
  
  def runQuery(query: OpenIeQuery, rlinking: Boolean):
      Set[ExtractionGroup[ReVerbExtraction]] = {
    val queryText = query.getQueryString(rlinking)
    solrExecutor.execute(queryText).toSet
  }
  
  def outputStats(query: OpenIeQuery, groupsWithRlinking: Set[ExtractionGroup[ReVerbExtraction]],
      groupsWithoutRlinking: Set[ExtractionGroup[ReVerbExtraction]], writer: BufferedWriter):
      Unit = {
    writer.write(query.getQueryString(false))
    writer.newLine()
    writer.write(query.getQueryString(true))
    writer.newLine()
    
    writer.write("Without linking: %d groups, %d sentences.".format(
      groupsWithoutRlinking.size,
      groupsWithoutRlinking.foldLeft(0)((sum, group) => sum + group.instances.size)
    ))
    writer.newLine()
    writer.write("   With linking: %d groups, %d sentences.".format(
      groupsWithRlinking.size,
      groupsWithRlinking.foldLeft(0)((sum, group) => sum + group.instances.size)
    ))
    writer.newLine()
    writer.newLine()
  }
  
  def outputSentences(query: OpenIeQuery, groupsWithRlinking: Set[ExtractionGroup[ReVerbExtraction]],
      groupsWithoutRlinking: Set[ExtractionGroup[ReVerbExtraction]], writer: BufferedWriter): Unit = {
    writer.write(query.getQueryString(false))
    writer.newLine()
    writer.write("  Unique groups without linking:")
    writer.newLine()
    (groupsWithoutRlinking -- groupsWithRlinking).foreach({ group =>
      writer.write("    (%s, %s, %s)".format(
        group.arg1.norm, group.rel.norm, group.arg2.norm
      ))
      writer.newLine()
      group.instances.take(5).foreach({instance =>
        val sentenceText = instance.extraction.sentenceText
        writer.write("      " + sentenceText)
        writer.newLine()
      })
    })
    
    writer.write("  Unique groups with linking: ")
    writer.newLine()
    (groupsWithRlinking -- groupsWithoutRlinking).foreach({ group =>
      writer.write("    (%s, %s, %s)".format(
        group.arg1.norm, group.rel.norm, group.arg2.norm
      ))
      writer.newLine()
      group.instances.take(2).foreach({instance =>
        writer.write("      " + instance.extraction.sentenceText)
        writer.newLine()
      })
    })
    writer.newLine()
  }
  
  def run(): Unit = {
    val queries = getTestQueries()
    val statsWriter = new BufferedWriter(new FileWriter(outputDir + QUERY_STATS_FILE))
    val sentenceWriter = new BufferedWriter(new FileWriter(outputDir + SENTENCES_FILE))
    
    queries.foreach({ query =>
      val groupsWithRlinking = runQuery(query, true)
      val groupsWithoutRlinking = runQuery(query, false)
      
      outputStats(query, groupsWithRlinking, groupsWithoutRlinking, statsWriter)
      outputSentences(query, groupsWithRlinking, groupsWithoutRlinking, sentenceWriter)
    })
    statsWriter.close()
    sentenceWriter.close()
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
