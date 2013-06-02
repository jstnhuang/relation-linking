package edu.washington.cs.knowitall.browser.hadoop.scoobi

import scala.Option.option2Iterable

import com.nicta.scoobi.Scoobi.{ComparableGrouping, DList, ScoobiApp, StringFmt, Tuple2Fmt, persist}
import com.nicta.scoobi.core.Reduction
import com.nicta.scoobi.io.text.{TextInput, TextOutput}

import edu.knowitall.collection.immutable.Interval
import edu.knowitall.openie.models.{ExtractionGroup, Instance, ReVerbExtraction, ReVerbExtractionGroup, ReVerbInstanceSerializer}
import edu.knowitall.tool.parse.ClearParser
import edu.knowitall.tool.srl.ClearSrl
import scopt.OptionParser

object ReverbSrlRunnerStaticVars {
  val clearParser = new ClearParser() 
  val clearSrl = new ClearSrl()
}

/**
 * Executes SRL over ReVerb extraction groups.
 */
object ReverbSrlRunner extends ScoobiApp {
  /**
   * A Reverb extraction group contains an (arg1, rel, arg2) tuple, entity links and types for the
   * args, a link for the relation, and a set of instances (sentences containing the tuple). For 
   * each sentence of each group, this job constructs a partial sentence using the original text
   * of (arg1, rel, arg2), and runs SRL on that to get the PropBank link. Then, it regroups the
   * sentences back (in case not every sentence in the group had the same relation link). 
   */
  def runSrl(groups: DList[String]): DList[String] = {
    import ReverbSrlRunnerStaticVars._
    
    val updatedGroups = groups.flatMap { line =>
      ReVerbExtractionGroup.deserializeFromString(line) match {
        case Some(group) => {
          group.instances.flatMap { instance =>
            val extraction = instance.extraction
            val sentenceText = extraction.sentenceText
            
            if (extraction.sentenceTokens.size > 80) {
              None
            } else {
              try {
                val graph = clearParser.dependencyGraph(sentenceText)
                val frames = clearSrl(graph);
                
                val relLink = if (frames.isEmpty) {
                  None
                } else {
                  val frameIndex = frames.lastIndexWhere({ frame =>
                    extraction.relInterval.superset(frame.relation.node.tokenInterval)
                  })
                  if (frameIndex < 0) {
                    None
                  } else {
                    Some(frames(frameIndex).relation.toString())
                  }
                }
                val key = List(
                  group.arg1.norm,
                  group.rel.norm,
                  group.arg2.norm,
                  ReVerbExtractionGroup.serializeEntity(group.arg1.entity),
                  ReVerbExtractionGroup.serializeEntity(group.arg2.entity),
                  ReVerbExtractionGroup.serializeTypeList(group.arg1.types),
                  ReVerbExtractionGroup.serializeTypeList(group.arg2.types),
                  relLink.getOrElse("X")
                ).mkString("\t")
                val value = ReVerbInstanceSerializer.serializeToString(instance)
                
                Some(key, List(value))
              } catch {
                case e: Error => {
                  System.err.println(
                    "ReverbSrlRunner: error processing %s: %s".format(sentenceText, e)
                  );
                  None
                }
                case e: Exception => {
                  System.err.println(
                    "ReverbSrlRunner: exception processing %s: %s".format(sentenceText, e)
                  );
                  None
                }
              }
            }
          }
        }
        case None => {
          System.err.println("ReverbSrlRunner: error parsing group: %s".format(line));
          None
        }
      }
    }.groupByKey
    .combine(Reduction.list[String])
    .map {
      case (key: String, instances: List[String]) => {
        key + "\t" + instances.mkString("\t")
      }
    }
    
    updatedGroups
  }
  
  /**
   * Entry point for the job.
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
      val outputGroups: DList[String] = runSrl(inputGroups)
      persist(TextOutput.toTextFile(outputGroups, outputPath));
    }
  }
}
