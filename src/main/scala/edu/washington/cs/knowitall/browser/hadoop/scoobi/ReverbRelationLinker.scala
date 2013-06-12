package edu.washington.cs.knowitall.browser.hadoop.scoobi

import com.nicta.scoobi.Scoobi.persist
import com.nicta.scoobi.application.ScoobiApp
import com.nicta.scoobi.core.DList
import com.nicta.scoobi.io.text.{TextInput, TextOutput}
import edu.knowitall.collection.immutable.Interval
import edu.knowitall.tool.postag.PostaggedToken
import edu.washington.cs.knowitall.WordNetUtils
import edu.washington.cs.knowitall.relation.Constants
import edu.washington.cs.knowitall.relation.linker.{SrlRelationLinker, VerbNetRelationLinker, WordNetRelationLinker}
import edu.washington.cs.knowitall.relation.linker.EntailmentDirection._
import scopt.OptionParser
import edu.knowitall.openie.models.ReVerbExtractionGroup
import edu.knowitall.openie.models.ReVerbInstanceSerializer
import com.nicta.scoobi.core.Reduction
import java.io.File

/**
 * Hadoop job that links a Reverb extraction group to its SRL sense, WordNet sense, and VerbNet
 * sense.
 */
object ReverbRelationLinker extends ScoobiApp {
  val BASE_PATH = new File("/scratch2/rlinking")
  val WORDNET_PATH = Constants.wordNetPath(BASE_PATH)
  val VERBNET_PATH = Constants.derbyDbUrl(new File(BASE_PATH, "relationlinking"))
  val srlLinker = SrlRelationLinker
  val wordNetUtils = new WordNetUtils(WORDNET_PATH)
  val wnLinker = new WordNetRelationLinker(wordNetUtils)
  val vnLinker = new VerbNetRelationLinker(VERBNET_PATH, wordNetUtils, Hypernym)
  
  def getLinks(phrase: Seq[PostaggedToken], context: Option[(Seq[PostaggedToken], Interval)]):
      (Option[String], Option[String], Set[String]) = {
    val srlLinksOpt = srlLinker.getRelationLinks(phrase, context)
    val wnLinksOpt = wnLinker.getRelationLinks(phrase, context)
    val vnLinksOpt = vnLinker.getRelationLinks(phrase, context)
    
    val srlLink = srlLinksOpt match {
      case Some((preHeadWords, links, postHeadWords)) => {
        Some(links.head)
      }
      case None => None
    }
    val wnLink = wnLinksOpt match {
      case Some((preHeadWords, links, postHeadWords)) => {
        Some(wordNetUtils.wordToString(links.head))
      }
      case None => None
    }
    val vnLinks = vnLinksOpt match {
      case Some((preHeadWords, links, postHeadWords)) => {
        links
      }
      case None => Set.empty[String]
    }
    (srlLink, wnLink, vnLinks)
  }

  def linkRelations(inputGroups: DList[String]): DList[String] = {
    val result = inputGroups.flatMap({ line =>
      val cleanLine = line.filterNot({c => c==0})
      ReVerbExtractionGroup.deserializeFromString(cleanLine) match {
        case Some(group) => {
          group.instances.flatMap { instance =>
            val extraction = instance.extraction
            val relTokens = extraction.relTokens
            val sentenceTokens = extraction.sentenceTokens
            
            if (extraction.sentenceTokens.size > 80) {
              None
            } else {
              try {
                val (srlLink, wnLink, vnLinks) = getLinks(
                  relTokens,
                  Some(sentenceTokens, extraction.relInterval)
                )
              
                val key = List(
                  group.arg1.norm,
                  group.rel.norm,
                  group.arg2.norm,
                  ReVerbExtractionGroup.serializeEntity(group.arg1.entity),
                  ReVerbExtractionGroup.serializeEntity(group.arg2.entity),
                  ReVerbExtractionGroup.serializeTypeList(group.arg1.types),
                  ReVerbExtractionGroup.serializeTypeList(group.arg2.types),
                  srlLink.getOrElse("X"),
                  wnLink.getOrElse("X"),
                  ReVerbExtractionGroup.serializeVnLinks(vnLinks)
                ).mkString("\t")
                val value = ReVerbInstanceSerializer.serializeToString(instance)
                Some(key, List(value))
              } catch {
                case e: Error => {
//                  System.err.println("ReverbRelationLinker: error processing %s: %s".format(
//                    extraction.sentenceText, e));
                  None
                }
                case e: Exception => {
//                  System.err.println("ReverbRelationLinker: error processing %s: %s".format(
//                    extraction.sentenceText, e));
                  None
                }
              }
            }
          }
        }
        case None => {
          System.err.println("ReverbRelationLinker: error parsing group: %s".format(line));
          None
        }
      }
    }).groupByKey
    .combine(Reduction.list[String])
    .map {
      case (key: String, instances: List[String]) => {
        key + "\t" + instances.mkString("\t")
      }
    }
    result
  }

  /**
   * Gathers inputs, launches the job, and persists the output.
   */
  def run() {
    var inputPath = ""
    var outputPath = ""
    
    val parser = new OptionParser() {
      arg(
        "inputPath",
        "HDFS input path to tab delimited extraction groups.",
        {str => inputPath = str}
      )
      arg(
        "outputPath",
        "HDFS output path.",
        {str => outputPath = str}
      )
    }
    
    if (parser.parse(args)) {
      val inputGroups: DList[String] = TextInput.fromTextFile(inputPath)
      val outputGroups: DList[String] = linkRelations(inputGroups)
      persist(TextOutput.toTextFile(outputGroups, outputPath));
    }
  }
}
