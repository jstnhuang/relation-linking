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
  val FEATURES_FILE = new File(outputDir, "features.tsv")
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
  
  def outputFeatures(writer: BufferedWriter, query: BenchmarkQuery, expandedQueryRel: QueryRel,
      groups: Set[REG], tags: TagMap, wordNetUtils: WordNetUtils,
      wordNetLinker: WordNetRelationLinker, entailmentGraph: EntailmentGraphDb) = {
    val queryString = query.toString()
    val queryRelString = query.rel.get
    val queryArg1 = QueryArg.fromString(query.arg1.getOrElse(""))
    val queryRel = QueryRel.fromString(query.rel.getOrElse(""))
    val queryArg2 = QueryArg.fromString(query.arg2.getOrElse(""))
    val relString = queryRel.getFirstRel.get
    val (arg1Tags, relTags, arg2Tags) = QueryExpander.tagQuery(queryArg1, relString, queryArg2)
    val queryWordNetLink = wordNetLinker.getWordRelationLinks(relTags).head
    val queryWordNetLinkString = wordNetUtils.wordToString(queryWordNetLink)
    val queryWordNetSynonyms = wordNetUtils.getSynonyms(queryWordNetLink)
    val queryWordNetLinkSet = if (queryWordNetSynonyms.isEmpty) {
      wordNetUtils.getHyponyms(queryWordNetLink)
    } else {
      queryWordNetSynonyms
    }
    var queryPaths = Map.empty[String, Set[(String, IWord)]]
    queryWordNetLinkSet.foreach({ wordNetSense =>
      val wordNetString = wordNetUtils.wordToString(wordNetSense)
      val verbNetSense2s = entailmentGraph.verbNetSensesFromWordNet(wordNetString)
      verbNetSense2s.foreach({ verbNetSense2 =>
        val verbNetSense1s = entailmentGraph.entailingVerbNetSenses(verbNetSense2)
        verbNetSense1s.foreach({verbNetSense1 =>
          var wordNetSenses = queryPaths.getOrElse(verbNetSense1, Set.empty[(String, IWord)])
          wordNetSenses += Tuple(verbNetSense2, wordNetSense)
          queryPaths += (verbNetSense1 -> wordNetSenses)
        })
      })
    })
    val queryVerbNetSense1s = queryPaths.keySet;
    
    groups.foreach({group =>
      val tuple = "(%s, %s, %s)".format(group.arg1.norm, group.rel.norm, group.arg2.norm)
      val tag = tags.getOrElse((queryString, tuple), "")
      val instance = group.instances.head
      val extraction = instance.extraction
      val tupleString = extraction.relText;
      val tupleWordNetLink = wordNetLinker.getWordRelationLinks(extraction.relTokens).head
      val tupleWordNetLinkString = wordNetUtils.wordToString(tupleWordNetLink)
      val tupleWordNetLinkSynonyms = wordNetUtils.getSynonyms(tupleWordNetLink) 
      val tupleWordNetLinkSet = if (tupleWordNetLinkSynonyms.isEmpty) {
        wordNetUtils.getHypernyms(tupleWordNetLink)
      } else {
        tupleWordNetLinkSynonyms
      }
      var tuplePaths = Map.empty[String, Set[IWord]]
      tupleWordNetLinkSet.foreach({ wordNetSense =>
        val wordNetString = wordNetUtils.wordToString(wordNetSense)
        val verbNetSenses = entailmentGraph.verbNetSensesFromWordNet(wordNetString)
        verbNetSenses.foreach({ verbNetSense =>
          var wordNetSenses = tuplePaths.getOrElse(verbNetSense, Set.empty[IWord])
          wordNetSenses += wordNetSense
          tuplePaths += (verbNetSense -> wordNetSenses)
        })
      })
      val tupleVerbNetSense1s = tuplePaths.keySet;
      
      val verbNetIntersection = queryVerbNetSense1s.intersect(tupleVerbNetSense1s)
      verbNetIntersection.foreach({ verbNetSense1 =>
        val tupleWordNetSenses = tuplePaths(verbNetSense1)
        val queryEntailments = queryPaths(verbNetSense1)
        val allPaths = tupleWordNetSenses.zip(queryEntailments)
        allPaths.foreach({ case (tupleSense, (verbNetSense2, querySense)) =>
          val tupleSenseString = wordNetUtils.wordToString(tupleSense)
          val querySenseString = wordNetUtils.wordToString(querySense)
          val path = List(tupleString, tupleWordNetLinkString, tupleSenseString, verbNetSense1,
            verbNetSense2, querySenseString, queryWordNetLinkString, queryRelString, tag)
          writer.write(path.mkString("\t"))
          writer.newLine()
        })
      })
    })
  }
  
  /**
   * Runs the experiment for a given set of query expanders.
   */
  def run(): Unit = {
//    val baselineExpander = BaselineQueryExpander
//    val srlExpander = SrlQueryExpander
    val wordNetUtils = new WordNetUtils(WORDNET_PATH)
//    val wordNetExpander = new WordNetQueryExpander(wordNetUtils)
    val verbNetExpander = new VerbNetQueryExpander(RELATION_DB_PATH, wordNetUtils)
//    val cleanExpander = new CleanQueryExpander(RELATION_DB_PATH)
//    val queryExpanders: Seq[QueryExpander] = List(
//      baselineExpander, srlExpander, wordNetExpander, verbNetExpander, cleanExpander
//    )
    val queryExpanders: Seq[QueryExpander] = List(verbNetExpander)
    
    val benchmarkQueries = getTestQueries()
    val tags = getTags(TAGS_DIR)
    
    val sentenceWriter = new BufferedWriter(new FileWriter(SENTENCES_FILE))
    val featuresWriter = new BufferedWriter(new FileWriter(FEATURES_FILE))
    
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
        if (expander == verbNetExpander) {
          outputFeatures(featuresWriter, benchmarkQuery, expansion, groups, tags,
            wordNetUtils, wordNetRelationLinker, entailmentGraph)
        }
      })
    })
    sentenceWriter.close()
    featuresWriter.close()
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
