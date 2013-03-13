package edu.washington.cs.knowitall.browser.hadoop.scoobi

import com.nicta.scoobi.Scoobi._
import edu.washington.cs.knowitall.browser.extraction.ReVerbExtractionGroup
import scopt.OptionParser
import edu.washington.cs.knowitall.tool.srl.ClearSrl
import edu.washington.cs.knowitall.tool.parse.ClearParser
import edu.washington.cs.knowitall.browser.extraction.ExtractionGroup
import edu.washington.cs.knowitall.browser.extraction.ReVerbExtractionGroup
import edu.washington.cs.knowitall.browser.extraction.Instance
import edu.washington.cs.knowitall.browser.extraction.ReVerbExtraction
import edu.washington.cs.knowitall.browser.extraction.ReVerbExtractionGroup

/**
 * Executes SRL over ReVerb extraction groups.
 */
object ReverbSrlRunner extends ScoobiApp {
  val clearParser = new ClearParser() 
  val clearSrl = new ClearSrl()
  
  def runSrl(groups: DList[String]): DList[String] = {
    def combineStuff (key: String, lines: Iterable[String]) = {
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
            val arg1Text = instance.extraction.arg1Text
            val relText = instance.extraction.relText
            val arg2Text = instance.extraction.arg2Text
            val sentence = List(arg1Text, relText, arg2Text).mkString(" ")
            System.out.println("sentence: " + sentence)
            val graph = clearParser.dependencyGraph(sentence)
            val frames = clearSrl(graph);
            val relLink = if (frames.isEmpty) {
              None
            } else {
              System.out.println("Frames: ")
              frames map println
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
          }
        }
        case None => {
          System.err.println("ReverbSrlRunner: error parsing group: %s".format(line));
          None
        }
      }
    }.groupByKey
    .map(keyValues => combineStuff(keyValues._1, keyValues._2))
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