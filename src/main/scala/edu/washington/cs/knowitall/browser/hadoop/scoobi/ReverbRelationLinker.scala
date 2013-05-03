package edu.washington.cs.knowitall.browser.hadoop.scoobi

import com.nicta.scoobi.Scoobi.persist
import com.nicta.scoobi.application.ScoobiApp
import com.nicta.scoobi.core.DList
import com.nicta.scoobi.io.text.{TextInput, TextOutput}
import scopt.OptionParser
import edu.washington.cs.knowitall.relation.linker.SrlRelationLinker
import edu.washington.cs.knowitall.relation.linker.RelationLinker
import edu.knowitall.openie.models.ReVerbExtractionGroup
import edu.washington.cs.knowitall.relation.linker.VerbNetRelationLinker
import edu.washington.cs.knowitall.relation.linker.WordNetRelationLinker
import edu.knowitall.openie.models.ReVerbInstanceSerializer
import edu.knowitall.tool.postag.PostaggedToken
import edu.washington.cs.knowitall.db.DerbyHandler
import edu.washington.cs.knowitall.relation.Constants
import edu.knowitall.collection.immutable.Interval
//
//object ReverbRelationLinkerStaticVars {
//  
//}

/**
 * Hadoop job that links a Reverb extraction group to its SRL sense, WordNet sense, and VerbNet
 * sense.
 */
object ReverbRelationLinker extends ScoobiApp {
  val derbyHandler = new DerbyHandler(Constants.DERBY_SERVER + Constants.RELATION_BASEPATH + Constants.VNTABLES);
  val srlLinker = SrlRelationLinker
  val wnLinker = new WordNetRelationLinker(Constants.RELATION_BASEPATH + Constants.WORDNET_DICT)
  val vnLinker = new VerbNetRelationLinker(derbyHandler, Constants.RELATION_BASEPATH + Constants.WORDNET_DICT)
  
  def getLinks(phrase: Seq[PostaggedToken], context: Option[(Seq[PostaggedToken], Interval)]):
      (Option[String], Option[String], Set[String]) = {
//    import ReverbRelationLinkerStaticVars._
    val srlLinks = srlLinker.getRelationLinks(phrase, context)
    val wnLinks = wnLinker.getRelationLinks(phrase, context)
    val vnLinks = vnLinker.getRelationLinks(phrase, context)
    
    val srlLink = if (!srlLinks.isEmpty) {
      Some(srlLinks.head)
    } else {
      None
    }
    val wnLink = if (!wnLinks.isEmpty) {
      Some(wnLinks.head)
    } else {
      None
    }
    (srlLink, wnLink, vnLinks)
  }
  
  def linkRelations(inputGroups: DList[String]): DList[String] = {
    inputGroups.flatMap({ line =>
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
                Some(key, value)
              } catch {
                case e: Error => {
                  System.err.println("ReverbRelationLinker: error processing %s: %s".format(
                    extraction.sentenceText, e));
                  e.printStackTrace();
                  None
                }
                case e: Exception => {
                  System.err.println("ReverbRelationLinker: error processing %s: %s".format(
                    extraction.sentenceText, e));
                  e.printStackTrace();
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
    .combine((instance1: String, instance2: String) => instance1 + "\t" + instance2)
    .map {
      case (key: String, instances: String) => {
        key + "\t" + instances
      }
    }
  }
  
  /**
   * Gathers inputs, launches the job, and persists the output.
   */
  def run() {
    var basePath = ""
    var inputPath = ""
    var outputPath = ""
    
    val parser = new OptionParser() {
      arg(
        "basePath",
        "Path with WordNet and VerbNet tables.",
        {str => basePath = str}
      )
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
