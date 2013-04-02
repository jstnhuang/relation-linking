package edu.washington.cs.knowitall.browser.hadoop.scoobi

import scala.Option.option2Iterable
import com.nicta.scoobi.Scoobi.ComparableGrouping
import com.nicta.scoobi.Scoobi.DList
import com.nicta.scoobi.Scoobi.ScoobiApp
import com.nicta.scoobi.Scoobi.StringFmt
import com.nicta.scoobi.Scoobi.TextInput
import com.nicta.scoobi.Scoobi.TextOutput
import com.nicta.scoobi.Scoobi.Tuple2Fmt
import com.nicta.scoobi.Scoobi.persist
import edu.knowitall.tool.parse.ClearParser
import edu.knowitall.tool.srl.ClearSrl
import edu.washington.cs.knowitall.browser.extraction.ExtractionGroup
import edu.washington.cs.knowitall.browser.extraction.ReVerbExtractionGroup
import scopt.OptionParser
import edu.washington.cs.knowitall.browser.extraction.Instance
import edu.washington.cs.knowitall.browser.extraction.ReVerbExtraction
import edu.washington.cs.knowitall.browser.extraction.ReVerbInstanceSerializer

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
            val arg1Text = instance.extraction.arg1Text
            val relText = instance.extraction.relText
            val arg2Text = instance.extraction.arg2Text
            val sentence = List(arg1Text, relText, arg2Text).mkString(" ")
            try {
              val graph = clearParser.dependencyGraph(sentence)
              val frames = clearSrl(graph);
              val relLink = if (frames.isEmpty) {
                None
              } else {
                Some(frames(0).relation.toString())
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
              
              Some(key, value)
            } catch {
              case e: Error => {
                System.err.println("ReverbSrlRunner: error processing " + sentence + ": " + e);
                None
              }
              case e: Exception => {
                System.err.println("ReverbSrlRunner: exception processing " + sentence + ": " + e);
                None
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
    .combine((instance1: String, instance2: String) => instance1 + "\t" + instance2)
    .map {
      case (key: String, instances: String) => {
        key + "\t" + instances
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
