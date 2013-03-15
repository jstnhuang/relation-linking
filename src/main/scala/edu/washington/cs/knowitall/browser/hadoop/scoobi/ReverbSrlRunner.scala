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

/**
 * Executes SRL over ReVerb extraction groups.
 */
object ReverbSrlRunner extends ScoobiApp {
  val clearParser = new ClearParser() 
  val clearSrl = new ClearSrl()
  
  /**
   * A Reverb extraction group contains an (arg1, rel, arg2) tuple, entity links and types for the
   * args, a link for the relation, and a set of instances (sentences containing the tuple). For 
   * each sentence of each group, this job constructs a partial sentence using the original text
   * of (arg1, rel, arg2), and runs SRL on that to get the PropBank link. Then, it regroups the
   * sentences back (in case not every sentence in the group had the same relation link). 
   */
  def runSrl(groups: DList[String]): DList[String] = {
    /**
     * Reducer -- the mapper emits groups with just one instance each, this combines the instances
     * into just one group.
     */
    def combineInstances (key: String, lines: Iterable[String]) = {
      var firstGroup = ReVerbExtractionGroup.deserializeFromString(lines.head).get;
      var instances = firstGroup.instances
      
      for (line <- lines) {
        val group = ReVerbExtractionGroup.deserializeFromString(line).get
        instances |= group.instances
      }
      
      val combined = new ExtractionGroup(
        firstGroup.arg1.norm,
        firstGroup.rel.norm,
        firstGroup.arg2.norm,
        firstGroup.arg1.entity,
        firstGroup.arg2.entity,
        firstGroup.arg1.types,
        firstGroup.arg2.types,
        firstGroup.rel.link,
        instances
      )
      ReVerbExtractionGroup.serializeToString(combined)
    }
    
    groups.flatMap { line =>
      ReVerbExtractionGroup.deserializeFromString(line) match {
        case Some(group) =>  {
          group.instances.flatMap { instance =>
            try {
              val arg1Text = instance.extraction.arg1Text
              val relText = instance.extraction.relText
              val arg2Text = instance.extraction.arg2Text
              val sentence = List(arg1Text, relText, arg2Text).mkString(" ")
              
              val graph = clearParser.dependencyGraph(sentence)
              val frames = clearSrl(graph);
              val relLink = if (frames.isEmpty) {
                None
              } else {
                Some(frames(0).relation.toString())
              }
              val updatedGroup = new ExtractionGroup(
                group.arg1.norm,
                group.rel.norm,
                group.arg2.norm,
                group.arg1.entity,
                group.arg2.entity,
                group.arg1.types,
                group.arg2.types,
                relLink,
                Set(instance)
              )
              val key = List(
                updatedGroup.arg1.norm,
                updatedGroup.rel.norm,
                updatedGroup.arg2.norm,
                if (updatedGroup.arg1.hasEntity) updatedGroup.arg1.entity.get.fbid else "X",
                if (updatedGroup.rel.hasLink) updatedGroup.rel.link else "X",
                if (updatedGroup.arg2.hasEntity) updatedGroup.arg2.entity.get.fbid else "X"
              ).mkString("-")
              Some(key, ReVerbExtractionGroup.serializeToString(updatedGroup))
            } catch {
              case e: Exception => None
            }
          }
        }
        case None => {
          System.err.println("ReverbSrlRunner: error parsing group: %s".format(line));
          None
        }
      }
    }.groupByKey
    .map(keyValues => combineInstances(keyValues._1, keyValues._2))
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
      persist(TextOutput.toTextFile(outputGroups, outputPath + "/"));
    }
  }
}