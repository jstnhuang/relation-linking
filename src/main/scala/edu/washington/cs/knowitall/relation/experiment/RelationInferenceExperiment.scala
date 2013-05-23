package edu.washington.cs.knowitall.relation.experiment

import java.io.{BufferedWriter, File, FileWriter}
import scala.io.Source
import edu.knowitall.openie.models.{ExtractionGroup, ReVerbExtraction}
import edu.washington.cs.knowitall.SolrQueryExecutor
import edu.washington.cs.knowitall.model.{OpenIeQuery, QueryRel}
import edu.washington.cs.knowitall.relation.Constants
import edu.washington.cs.knowitall.relation.expander.{BaselineQueryExpander, CleanQueryExpander, QueryExpander, SrlQueryExpander, VerbNetQueryExpander, WordNetQueryExpander}
import scopt.OptionParser
import edu.washington.cs.knowitall.WordNetUtils
import edu.washington.cs.knowitall.relation.linker.VerbNetRelationLinker
import edu.washington.cs.knowitall.relation.linker.WordNetRelationLinker
import edu.washington.cs.knowitall.db.DerbyHandler
import edu.washington.cs.knowitall.relation.EntailmentGraphDb
import edu.washington.cs.knowitall.model.QueryArg
import edu.mit.jwi.item.IWord

class RelationInferenceExperiment(solrUrl: String, inputDir: String, outputDir: String) {
  val solrExecutor = new SolrQueryExecutor(solrUrl)
  val WORDNET_PATH = Constants.wordNetPath(inputDir)
  val RELATION_DB_PATH = Constants.relationLinkingDbPath("/scratch2/rlinking")
  val BENCHMARK_QUERIES_FILE = new File(inputDir, "benchmark-queries.tsv")
  val TAGS_DIR = new File(inputDir, "tags")
  val SENTENCES_FILE = new File(outputDir, "sentences.tsv")
  type REG = ExtractionGroup[ReVerbExtraction];
  type TagMap = scala.collection.mutable.Map[(String, String), String];
  
  /**
   * Reads test queries from input file.
   */
  def getTestQueries(): Seq[BenchmarkQuery] = {
    var testQueries = List[BenchmarkQuery]()
    Source.fromFile(BENCHMARK_QUERIES_FILE).getLines().foreach({ line =>
      testQueries = BenchmarkQuery.fromLine(line)::testQueries
    })
    testQueries
  }

  /**
   * Reads files from the tags directory and stores all the tags. Also reports if there are
   * any contradictary tags.
   */
  def getTags(tagDir: File): TagMap = {
    val files = tagDir.listFiles.filter(_.getName.endsWith(".tsv"))
    var tags: TagMap = scala.collection.mutable.Map.empty[(String, String), String]
    files.foreach({ file =>
      Source.fromFile(file).getLines().foreach({ line =>
        val result = RelationInferenceResult.fromString(line.trim())
        val tagKey = result.getTagKey()
        val tag = result.getTag()
        if (tag.contains(tagKey) && tags.get(tagKey) != tag) {
          throw new RuntimeException("Contradicting tags in %s: %s has tags %s and %s".format(
            file.getName, tagKey, tags.get(tagKey), tag
          ))
        }
        if (tag != "") {
          tags += (tagKey -> tag)
        }
      })
    });
    tags
  }
  
  def runQuery(query: OpenIeQuery): Set[REG] = {
    val queryText = query.getQueryString()
    solrExecutor.execute(queryText).toSet
  }
  
  /**
   * Output lines of the form: system name, benchmark query, expanded query, tuple, tag, sentence,
   * tuple links.
   */
  def outputSentences(writer: BufferedWriter, name: String, testQuery: BenchmarkQuery,
      expandedQuery: QueryRel, groups: Set[REG], tags: TagMap) {
    val testQueryStr = testQuery.toString()
    val expandedQueryStr = expandedQuery.toString()
    groups.foreach({ group =>
      val tuple = "(%s, %s, %s)".format(group.arg1.norm, group.rel.norm, group.arg2.norm)
      val tupleLinks = "SRL: %s; WordNet: %s; VerbNet: %s".format(
        group.rel.srlLink.getOrElse("X"),
        group.rel.wnLink.getOrElse("X"),
        if (group.rel.vnLinks.isEmpty) { "X" } else { group.rel.vnLinks.mkString(", ") } 
      )
      
      // Only get one sentence for now.
      val sentence = group.instances.head.extraction.sentenceText.trim()
      val tag = tags.getOrElse((testQueryStr, tuple), "")
      val result = new RelationInferenceResult(
        name, testQueryStr, expandedQueryStr, tuple, tag, sentence, tupleLinks
      )
      writer.write(result.toString())
      writer.newLine()
    })
  }
  
  /**
   * Runs the experiment for a given set of query expanders.
   */
  def run(): Unit = {
    val baselineExpander = BaselineQueryExpander
    val srlExpander = SrlQueryExpander
    val wordNetUtils = new WordNetUtils(WORDNET_PATH)
    val wordNetExpander = new WordNetQueryExpander(wordNetUtils)
    val verbNetExpander = new VerbNetQueryExpander(RELATION_DB_PATH, wordNetUtils)
    val cleanExpander = new CleanQueryExpander(RELATION_DB_PATH)
    val queryExpanders: Seq[QueryExpander] = List(
      baselineExpander, srlExpander, wordNetExpander, verbNetExpander, cleanExpander
    )
    
    val benchmarkQueries = getTestQueries()
    val tags = getTags(TAGS_DIR)
    
    val sentenceWriter = new BufferedWriter(new FileWriter(SENTENCES_FILE))
    
    val wordNetRelationLinker = new WordNetRelationLinker(wordNetUtils);
    val derbyHandler = new DerbyHandler(RELATION_DB_PATH)
    val entailmentGraph = new EntailmentGraphDb(derbyHandler);
    
    queryExpanders.foreach({ expander =>
      val systemName = expander.getName()
      benchmarkQueries.foreach({ benchmarkQuery =>
        val query = expander.expandQuery(benchmarkQuery)
        val (groups, expansion) = if (query == null) {
          (Set.empty[REG], new QueryRel())
        } else {
          (runQuery(query), query.getQueryRel)
        }
        outputSentences(sentenceWriter, systemName, benchmarkQuery, expansion, groups, tags)
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
