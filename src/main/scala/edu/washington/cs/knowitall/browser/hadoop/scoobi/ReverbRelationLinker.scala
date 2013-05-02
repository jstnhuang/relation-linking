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

/**
 * Hadoop job that links a Reverb extraction group to its SRL sense, WordNet sense, and VerbNet
 * sense.
 */
object ReverbRelationLinker extends ScoobiApp {
  val MAX_SENTENCE_LENGTH = 80
  
  def getLinks(srlLinker: RelationLinker, wnLinker: RelationLinker, vnLinker: RelationLinker,
      phrase: Seq[PostaggedToken], context: Option[Seq[PostaggedToken]]):
      (Option[String], Option[String], Set[String]) = {
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
  
  def linkRelations(srlLinker: RelationLinker, wnLinker: RelationLinker, vnLinker: RelationLinker,
      inputGroups: DList[String]): DList[String] = {
    inputGroups.flatMap({ line =>
      ReVerbExtractionGroup.deserializeFromString(line) match {
        case Some(group) => {
          group.instances.flatMap { instance =>
            val extraction = instance.extraction
            val relTokens = extraction.relTokens
            val sentenceTokens = extraction.sentenceTokens
            
            if (extraction.sentenceTokens.size > MAX_SENTENCE_LENGTH) {
              None
            } else {
              val (srlLink, wnLink, vnLinks) = getLinks(srlLinker, wnLinker, vnLinker, relTokens,
                Some(sentenceTokens))
            
              val key = List(
                group.arg1.norm,
                group.arg2.norm,
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
      val srlLinker = SrlRelationLinker
      val wnLinker = new WordNetRelationLinker(basePath)
      val vnLinker = new VerbNetRelationLinker(basePath)
      val outputGroups: DList[String] = linkRelations(srlLinker, wnLinker, vnLinker, inputGroups)
      persist(TextOutput.toTextFile(outputGroups, outputPath));
    }
  }
}