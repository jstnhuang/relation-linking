package edu.washington.cs.knowitall.relation.experiment

import java.io.{BufferedWriter, File, FileWriter}

import scala.io.Source

import edu.knowitall.openie.models.{ExtractionGroup, ReVerbExtraction, ReVerbExtractionGroup, ReVerbInstanceSerializer}
import edu.washington.cs.knowitall.SolrQueryExecutor
import edu.washington.cs.knowitall.model.{OpenIeQuery, QueryArg, QueryRel}
import scopt.OptionParser

/**
 * Retrieves a subset of ReverbExtractionGroups by querying Solr, and writes them out to a file.
 */
class CorpusSelector(solrUrl: String, inputDir: String, outputDir: String) {
  val solrExecutor = new SolrQueryExecutor(solrUrl)
  val BENCHMARK_QUERIES_PATH = List(inputDir, "benchmark-queries.tsv").mkString(File.separator)
  val REG_PATH = List(outputDir, "rel_inf_regs.tsv").mkString(File.separator)
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
   * Output ReverbExtractionGroups found.
   */
  def outputGroups(writer: BufferedWriter, groups: Set[REG]): Unit = {
    groups.foreach({ group =>
      val line = List(
        group.arg1.norm,
        group.rel.norm,
        group.arg2.norm,
        ReVerbExtractionGroup.serializeEntity(group.arg1.entity),
        ReVerbExtractionGroup.serializeEntity(group.arg2.entity),
        ReVerbExtractionGroup.serializeTypeList(group.arg1.types),
        ReVerbExtractionGroup.serializeTypeList(group.arg2.types),
        group.rel.srlLink.getOrElse("X"),
        group.rel.wnLink.getOrElse("X"),
        ReVerbExtractionGroup.serializeVnLinks(group.rel.vnLinks),
        group.instances.iterator.map({ instance =>
          ReVerbInstanceSerializer.serializeToString(instance)
        }).mkString("\t")
      ).mkString("\t")
      writer.write(line)
      writer.newLine()
    })
  }
  
  /**
   * Runs the corpus selector.
   */
  def run(): Unit = {
    val benchmarkQueries = getTestQueries()
    
    val regWriter = new BufferedWriter(new FileWriter(REG_PATH))
    benchmarkQueries.foreach({ benchmarkQuery =>
      val query = new OpenIeQuery(
        QueryArg.fromString(benchmarkQuery.arg1.getOrElse("")),
        new QueryRel(),
        QueryArg.fromString(benchmarkQuery.arg2.getOrElse(""))
      )
      val groups = runQuery(query)
      outputGroups(regWriter, groups)
    })
    regWriter.close()
  }
}

object CorpusSelector {
  def main(args: Array[String]): Unit = {
    var solrUrl = ""
    var inputDir = "."
    var outputDir = "."
    
    val parser = new OptionParser() {
      arg("solrUrl", "URL of Solr instance to query.", {str => solrUrl = str})
      arg("inputDir", "Directory of benchmark tuples.", {str => inputDir = str})
      arg("outputDir", "Directory to put output files.", {str => outputDir = str})
    }
    
    if (!parser.parse(args)) {
      return
    }
    
    val selector = new CorpusSelector(solrUrl, inputDir, outputDir)
    selector.run()
  }
}
