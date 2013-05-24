package edu.washington.cs.knowitall.relation.experiment

import java.io.{BufferedWriter, File, FileWriter}

import scala.io.Source

import edu.knowitall.openie.models.{ExtractionGroup, ReVerbExtraction}
import edu.mit.jwi.item.IWord
import edu.washington.cs.knowitall.{SolrQueryExecutor, WordNetUtils}
import edu.washington.cs.knowitall.db.DerbyHandler
import edu.washington.cs.knowitall.model.{OpenIeQuery, QueryArg, QueryRel}
import edu.washington.cs.knowitall.relation.{Constants, EntailmentGraphDb}
import edu.washington.cs.knowitall.relation.expander.{QueryExpander, VerbNetQueryExpander}
import edu.washington.cs.knowitall.relation.linker.WordNetRelationLinker
import scopt.OptionParser

case class Features(tupleString: String, tupleWordNetLink: IWord, tupleSense: IWord,
    verbNetSense1: String, wordNetSense1: IWord, wordNetSense2: IWord, verbNetSense2: String,
    querySense: IWord, queryWordNetLink: IWord, queryRelString: String, tupleSetType: String,
    edgeType: String, querySetType: String, tag: String) {
  val tupleEdgeType = if(tupleWordNetLink.equals(tupleSense)) {
    "identity"
  } else if (tupleSetType == "synonym") {
    "synonym"
  } else {
    "entailment"
  }
  val graphEdgeType = if(wordNetSense1.equals(wordNetSense2)) {
    "identity"
  } else if (edgeType == "synonym") {
    "synonym"
  } else {
    "entailment"
  }
  val queryEdgeType = if(queryWordNetLink.equals(querySense)) {
    "identity"
  } else if (querySetType == "synonym") {
    "synonym"
  } else {
    "entailment"
  }
  
  def trace(wordNetUtils: WordNetUtils): List[String] = {
    val tupleWordNetLinkString = wordNetUtils.wordToString(tupleWordNetLink, tagCount=true)
    val tupleSenseString = "%s (%s)".format(
      wordNetUtils.wordToString(tupleSense, tagCount=true),
      tupleEdgeType
    )
    val graphWordNetSense1String = wordNetUtils.wordToString(wordNetSense1, tagCount=true)
    val graphWordNetSense2String = "%s (%s)".format(
      wordNetUtils.wordToString(wordNetSense2, tagCount=true),
      graphEdgeType
    )
    val querySenseString = wordNetUtils.wordToString(querySense, tagCount=true)
    val queryWordNetLinkString = "%s (%s)".format(
      wordNetUtils.wordToString(queryWordNetLink, tagCount=true),
      queryEdgeType
    )
    List(tupleString, tupleWordNetLinkString, tupleSenseString, verbNetSense1,
      graphWordNetSense1String, graphWordNetSense2String, verbNetSense2, querySenseString,
      queryWordNetLinkString, queryRelString, tag)
  }
  
  /**
   * Feature set with all the WordNet counts and edge types.
   */
  def features(wordNetUtils: WordNetUtils): List[String] = {
    val counts = List(tupleWordNetLink, tupleSense, wordNetSense1, wordNetSense2, querySense,
      queryWordNetLink).map(wordNetUtils.getTagCount(_).toString)
    val edgeTypes = List(tupleEdgeType, graphEdgeType, queryEdgeType)
    counts ++ edgeTypes ++ List(tag)
  }
  
  /**
   * Feature set with: # of synonyms, # of entailments, # of graph sense changes, WordNet counts for
   * the tuple and query links, and the overall number of changes to WordNet senses along the path.
   */
  def features2(wordNetUtils: WordNetUtils): List[String] = {
    val synonymCount = List(tupleEdgeType, graphEdgeType, queryEdgeType).map({ edgeType =>
      if (edgeType == "synonym") { 1 } else { 0 }
    }).reduce(_ + _)
    val entailmentCount = List(tupleEdgeType, graphEdgeType, queryEdgeType).map({ edgeType =>
      if (edgeType == "entailment") { 1 } else { 0 }
    }).reduce(_ + _)
    val graphSenseChanges = List(
      tupleSense.equals(wordNetSense1), wordNetSense2.equals(querySense)
    ).map(if (_) { 1 } else { 0 }).reduce(_ + _)
    val tupleLinkCount = wordNetUtils.getTagCount(tupleWordNetLink)
    val queryLinkCount = wordNetUtils.getTagCount(queryWordNetLink)
    val wordNetSenseChanges = List(
      tupleWordNetLink.equals(tupleSense), tupleSense.equals(wordNetSense1),
      wordNetSense1.equals(wordNetSense2), wordNetSense2.equals(querySense),
      querySense.equals(queryWordNetLink)
    ).map(if (_) { 1 } else { 0 }).reduce(_ + _)
    List(synonymCount, entailmentCount, graphSenseChanges, tupleLinkCount, queryLinkCount,
      wordNetSenseChanges).map(_.toString) ++ List(tag)
  }
}

class FeatureExtractor(solrUrl: String, inputDir: String, outputDir: String) {
  val solrExecutor = new SolrQueryExecutor(solrUrl)
  val WORDNET_PATH = Constants.wordNetPath(inputDir)
  val wordNetUtils = new WordNetUtils(WORDNET_PATH)
  val RELATION_DB_PATH = Constants.relationLinkingDbPath("/scratch2/rlinking")
  val BENCHMARK_QUERIES_FILE = new File(inputDir, "benchmark-queries.tsv")
  val VN_TRACE_FILE = new File(inputDir, "vn_trace.txt")
  val TAGS_DIR = new File(inputDir, "tags")
  val TRACE_FILE = new File(outputDir, "trace.tsv")
  val FEATURES_FILE = new File(outputDir, "features.tsv")
  val FEATURES2_FILE = new File(outputDir, "features2.tsv")
  type REG = ExtractionGroup[ReVerbExtraction];
  type TagMap = scala.collection.mutable.Map[(String, String), String];
  type EntailmentGraphTrace = Map[(String, String), Set[(IWord, IWord, String)]]
  
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
  
  def getEntailmentGraphTrace(): EntailmentGraphTrace = {
    var trace = Map.empty[(String, String), Set[(IWord, IWord, String)]]
    Source.fromFile(VN_TRACE_FILE).getLines().foreach({ line =>
      val columns = "\t".r.split(line).map(_.trim())
      val vn1 = columns(0)
      val vn2 = columns(1)
      val wn1 = columns(4)
      val wnSynonym = columns(5)
      val wordSense1 = wordNetUtils.wordFromString(wn1)
      val (wordSense2, edgeType) = if(wnSynonym != "") {
        (wordNetUtils.wordFromString(wnSynonym), "synonym")
      } else {
        val wnHypernym = columns(6)
        (wordNetUtils.wordFromString(wnHypernym), "hypernym")
      }
      val key = (vn1, vn2)
      var path = trace.getOrElse(key, Set.empty[(IWord, IWord, String)])
      path += Tuple(wordSense1, wordSense2, edgeType)
      trace += (key -> path)
    })
    trace
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
          val tagOutput = if (tag == "1") {
            "correct"
          } else if (tag == "0") {
            "incorrect"
          } else {
            "bad"
          }
          tags += (tagKey -> tagOutput)
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
   * Extracts features from a query and a list of results.
   */
  def getFeatures(query: BenchmarkQuery, expandedQueryRel: QueryRel, groups: Set[REG], tags: TagMap,
      wordNetLinker: WordNetRelationLinker, entailmentGraph: EntailmentGraphDb,
      graphTrace: EntailmentGraphTrace): Set[Features] = {
    val queryRelString = query.rel.get
    val queryArg1 = QueryArg.fromString(query.arg1.getOrElse(""))
    val queryRel = QueryRel.fromString(query.rel.getOrElse(""))
    val queryArg2 = QueryArg.fromString(query.arg2.getOrElse(""))
    val relString = queryRel.getFirstRel.get
    val (arg1Tags, relTags, arg2Tags) = QueryExpander.tagQuery(queryArg1, relString, queryArg2)
    val queryWordNetLink = wordNetLinker.getWordRelationLinks(relTags).head
    val queryWordNetSynonyms = wordNetUtils.getSynonyms(queryWordNetLink)
    val (queryWordNetLinkSet, querySetType) = if (queryWordNetSynonyms.isEmpty) {
      (wordNetUtils.getHyponyms(queryWordNetLink), "troponym")
    } else {
      (queryWordNetSynonyms, "synonym")
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
    val queryString = query.toString()
    
    groups.flatMap({ group =>
      val tuple = "(%s, %s, %s)".format(group.arg1.norm, group.rel.norm, group.arg2.norm)
      val tag = tags.getOrElse((queryString, tuple), "")
      val extraction = group.instances.head.extraction
      val tupleString = extraction.relText;
      val tupleWordNetLink = wordNetLinker.getWordRelationLinks(extraction.relTokens).head
      val tupleWordNetLinkSynonyms = wordNetUtils.getSynonyms(tupleWordNetLink) 
      val (tupleWordNetLinkSet, tupleSetType) = if (tupleWordNetLinkSynonyms.isEmpty) {
        (wordNetUtils.getHypernyms(tupleWordNetLink), "hypernym")
      } else {
        (tupleWordNetLinkSynonyms, "synonym")
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
      
      verbNetIntersection.flatMap({ verbNetSense1 =>
        val tupleWordNetSenses = tuplePaths(verbNetSense1)
        val queryEntailments = queryPaths(verbNetSense1)
        tupleWordNetSenses.flatMap({ tupleSense =>
          val tupleSenseCount = wordNetUtils.getTagCount(tupleSense)
          queryEntailments.flatMap({ case(verbNetSense2, querySense) =>
            val querySenseCount = wordNetUtils.getTagCount(querySense)
            val graphPaths = graphTrace(verbNetSense1, verbNetSense2)
            graphPaths.map({ case(wordNetSense1, wordNetSense2, edgeType) =>              
              Features(tupleString, tupleWordNetLink, tupleSense, verbNetSense1,
                wordNetSense1, wordNetSense2, verbNetSense2, querySense, queryWordNetLink,
                queryRelString, tupleSetType, edgeType, querySetType, tag)
            })
          })
        })
      })
    })
  }
  
  def outputTrace(writer: BufferedWriter, featuresList: Set[Features]) = {
    featuresList.foreach({ features =>
      writer.write(features.trace(wordNetUtils).mkString("\t"))
      writer.newLine()
    })
  }
  
  def outputFeatures(writer: BufferedWriter, featuresList: Set[Features]) = {
    featuresList.foreach({ features =>
      writer.write(features.features(wordNetUtils).mkString("\t"))
      writer.newLine()
    })
  }
  
  def outputFeatures2(writer: BufferedWriter, featuresList: Set[Features]) = {
    featuresList.foreach({ features =>
      writer.write(features.features2(wordNetUtils).mkString("\t"))
      writer.newLine()
    })
  }
  
  /**
   * Runs the experiment for a given set of query expanders.
   */
  def run(): Unit = {
    val expander = new VerbNetQueryExpander(RELATION_DB_PATH, wordNetUtils)
    
    val benchmarkQueries = getTestQueries()
    val tags = getTags(TAGS_DIR)
    
    val traceWriter = new BufferedWriter(new FileWriter(TRACE_FILE))
    val featuresWriter = new BufferedWriter(new FileWriter(FEATURES_FILE))
    val features2Writer = new BufferedWriter(new FileWriter(FEATURES2_FILE))
    
    val wordNetRelationLinker = new WordNetRelationLinker(wordNetUtils);
    val derbyHandler = new DerbyHandler(RELATION_DB_PATH)
    val entailmentGraph = new EntailmentGraphDb(derbyHandler);
    val graphTrace = getEntailmentGraphTrace()
    
    benchmarkQueries.foreach({ benchmarkQuery =>
      val query = expander.expandQuery(benchmarkQuery)
      val (groups, expansion) = if (query == null) {
        (Set.empty[REG], new QueryRel())
      } else {
        (runQuery(query), query.getQueryRel)
      }
      val featuresList = getFeatures(benchmarkQuery, expansion, groups, tags, wordNetRelationLinker,
        entailmentGraph, graphTrace)
      outputTrace(traceWriter, featuresList)
      outputFeatures(featuresWriter, featuresList)
      outputFeatures2(features2Writer, featuresList)
    })
    traceWriter.close()
    featuresWriter.close()
    features2Writer.close()
  }
}

object FeatureExtractor {
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
    
    val experiment = new FeatureExtractor(solrUrl, inputDir, outputDir)
    experiment.run()
  }
}
