package edu.washington.cs.knowitall.relation.experiment

import java.io.BufferedWriter
import scala.io.Source
import edu.knowitall.openie.models.{ExtractionGroup, ReVerbExtraction}
import edu.washington.cs.knowitall.SolrQueryExecutor
import edu.washington.cs.knowitall.model.OpenIeQuery
import edu.washington.cs.knowitall.relation.expander.{BaselineQueryExpander, QueryExpander, SrlQueryExpander, VerbNetQueryExpander}
import scopt.OptionParser
import java.io.FileWriter
import java.io.File
import edu.washington.cs.knowitall.relation.Constants
import edu.washington.cs.knowitall.relation.expander.WordNetQueryExpander
import edu.washington.cs.knowitall.model.QueryRel

class RelationInferenceExperiment (solrUrl: String, inputDir: String, outputDir: String) {
  val solrExecutor = new SolrQueryExecutor(solrUrl)
  val WORDNET_PATH = Constants.wordNetPath(inputDir)
  val VERBNET_PATH = Constants.verbNetDbPath(inputDir)
  val BENCHMARK_QUERIES_PATH = List(inputDir, "benchmark-queries.tsv").mkString(File.separator)
  val SENTENCES_PATH = List(outputDir, "sentences.txt").mkString(File.separator)
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
  
  def runQuery(query: OpenIeQuery): Set[REG] = {
    val queryText = query.getQueryString()
    solrExecutor.execute(queryText).toSet
  }
  
  /**
   * Output lines of the form: system name, benchmark query, expanded query, tuple, tag, sentence
   */
  def outputSentences(writer: BufferedWriter, name: String, testQuery: BenchmarkQuery,
      expandedQuery: QueryRel, groups: Set[REG]) {
    val tag = ""
    groups.foreach({ group =>
      val tuple = "(%s, %s, %s)".format(group.arg1.norm, group.rel.norm, group.arg2.norm)
      group.instances.foreach({ instance =>
        val sentence = instance.extraction.sentenceText
        writer.write("%s\t%s\t%s\t%s\t%s\t%s".format(
          name, testQuery, expandedQuery, tuple, tag, sentence
        ))
        writer.newLine()
      })
    })
  }
  
  /**
   * Runs the experiment for a given set of query expanders.
   */
  def run(): Unit = {
    val baselineExpander = BaselineQueryExpander
//    val srlExpander = SrlQueryExpander
    val wordNetExpander = new WordNetQueryExpander(WORDNET_PATH)
    val verbNetExpander = new VerbNetQueryExpander(VERBNET_PATH, WORDNET_PATH)
    val queryExpanders: Seq[QueryExpander] = List(baselineExpander, wordNetExpander, verbNetExpander)
    val benchmarkQueries = getTestQueries()
    
    val sentenceWriter = new BufferedWriter(new FileWriter(SENTENCES_PATH))
    queryExpanders.foreach({ expander =>
      val systemName = expander.getName()
      benchmarkQueries.foreach({ benchmarkQuery =>
        val query = expander.expandQuery(benchmarkQuery)
        val (groups, expansion) = if (query == null) {
          (Set.empty[REG], new QueryRel())
        } else {
          (runQuery(query), query.getQueryRel)
        }
        outputSentences(sentenceWriter, systemName, benchmarkQuery, expansion, groups)
      })
    })
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
      arg("inputDir", "Directory of VerbNet tables and WordNet.", {str => inputDir = str})
      arg("outputDir", "Directory to put output files.", {str => outputDir = str})
    }
    
    if (!parser.parse(args)) {
      return
    }
    
    val experiment = new RelationInferenceExperiment(solrUrl, inputDir, outputDir)
    experiment.run()
  }
}
