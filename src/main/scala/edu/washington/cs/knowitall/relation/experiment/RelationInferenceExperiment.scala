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
import edu.washington.cs.knowitall.OpenIeQuery
import edu.washington.cs.knowitall.browser.extraction.ExtractionArgument
import edu.washington.cs.knowitall.browser.extraction.ExtractionGroup
import edu.washington.cs.knowitall.browser.extraction.ExtractionRelation
import edu.washington.cs.knowitall.browser.extraction.FreeBaseEntity
import edu.washington.cs.knowitall.browser.extraction.FreeBaseType
import edu.washington.cs.knowitall.browser.extraction.Instance
import edu.washington.cs.knowitall.browser.extraction.ReVerbExtraction
import scopt.OptionParser
import edu.washington.cs.knowitall.SolrQueryExecutor
import java.io.FileWriter
import java.io.BufferedWriter

class RelationInferenceExperiment (solrUrl: String, inputDir: String, outputDir: String) {
  val solrExecutor = new SolrQueryExecutor(solrUrl)
  val BENCHMARK_QUERIES_FILE = "benchmark-queries.tsv"
  val OUTPUT_FILE = "output.txt"
  
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
  
  def run(): Unit = {
    val queries = getTestQueries()
    val writer = new BufferedWriter(new FileWriter(outputDir + OUTPUT_FILE))
    queries.foreach({ query =>
      val queryTextWithRlinking = query.getQueryString(true)
      writer.write(queryTextWithRlinking)
      writer.newLine()
      
      val groups = solrExecutor.execute(queryTextWithRlinking)
      groups.foreach({ group =>
        writer.write("(%s, %s, %s)".format(group.arg1.norm, group.rel.norm, group.arg2.norm))
        writer.newLine()
      })
      writer.newLine()
    })
    writer.close()
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