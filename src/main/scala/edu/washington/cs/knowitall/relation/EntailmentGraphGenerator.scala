package edu.washington.cs.knowitall.relation

import java.io.File
import scala.io.Source
import edu.mit.jwi.IDictionary
import edu.mit.jwi.item.IWord
import edu.washington.cs.knowitall.WordNetUtils
import edu.washington.cs.knowitall.model.PropbankSense
import scopt.OptionParser
import edu.mit.jwi.item.POS

class EntailmentGraphGenerator(wordNetUtils: WordNetUtils) {
  /**
   * Given a file with rows like:
   *   elaborate.01 elaborate   1
   *   elaborate.01 elaborate   4
   *   
   * Get a map from Propbank senses to a set of WordNet senses.
   */
  def processPbToWn(pbToWnPath: String): Map[PropbankSense, Set[IWord]] = {
    val source = Source.fromFile(pbToWnPath)
    val lines = source.getLines.toList
    lines.groupBy({line =>
      val split = "\t".r.split(line)
      PropbankSense.fromString(split(0))
    }).mapValues({ samePbLines =>
      samePbLines.map({ samePbLine =>
        val split = "\t".r.split(samePbLine)
        wordNetUtils.getWordSense(split(1), POS.VERB, split(2).toInt)
      }).toSet
    })
  }
  
  /**
   * Given a file with rows like:
   *   bend 5   bend.01
   *   bend 5   bend.02
   *   
   * Get a map from Propbank senses to a set of WordNet senses.
   */
  def processWnToPb(wnToPbPath: String): Map[IWord, Set[PropbankSense]] = {
    val source = Source.fromFile(wnToPbPath)
    val lines = source.getLines.toList
    lines.groupBy({line =>
      val split = "\t".r.split(line)
      wordNetUtils.getWordSense(split(0), POS.VERB, split(1).toInt)
    }).mapValues({ sameWnLines =>
      sameWnLines.map({ sameWnLine =>
        val split = "\t".r.split(sameWnLine)
        PropbankSense.fromString(split(2))
      }).toSet
    })
  }
  
  def generateEntailmentGraph(): EntailmentGraph = {
    new EntailmentGraph
  }
}

object EntailmentGraphGenerator {
  def main(args: Array[String]): Unit = {
    var inputDir = "."
    var outputDir = "."
    
    val parser = new OptionParser() {
      arg("inputDir", "Directory of input files.", {str => inputDir = str})
      arg("outputDir", "Directory of output files.", {str => outputDir = str})
    }
    if (!parser.parse(args)) return
    
    val wordNetPath = Seq(inputDir, "WordNet-3.0", "dict").mkString(File.separator)
    val wordNetUtils = new WordNetUtils(wordNetPath)
    val generator = new EntailmentGraphGenerator(wordNetUtils)
    
    val pbToWnPath = Seq(inputDir, "pb-wn.tsv").mkString(File.separator)
    val pbToWn = generator.processPbToWn(pbToWnPath)
    
    val wnToPbPath = Seq(inputDir, "wn-pb.tsv").mkString(File.separator)
    val wnToPb = generator.processWnToPb(wnToPbPath)
    
    val graph = generator.generateEntailmentGraph()
  }
}