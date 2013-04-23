package edu.washington.cs.knowitall.relation

import java.io.BufferedWriter
import java.io.File
import scala.io.Source
import edu.mit.jwi.item.IWord
import edu.mit.jwi.item.POS
import edu.washington.cs.knowitall.WordNetUtils
import scopt.OptionParser
import java.io.FileWriter
import edu.washington.cs.knowitall.model.VerbNetSense

class EntailmentGraphGenerator(vnToWnPath: String, wnToVnPath: String, wordNetPath: String) {
  val wordNetUtils = new WordNetUtils(wordNetPath)
  val vnToWn = processVnToWn()
  val wnToVn = processWnToVn()
    
  /**
   * Given a file with rows like:
   *   vn_verb vn_sense vn_gloss wn_word wn_sense
   *   
   * Get a map from VerbNet senses to a set of WordNet senses.
   */
  private def processVnToWn(): Map[VerbNetSense, Set[IWord]] = {
    val source = Source.fromFile(vnToWnPath)
    val lines = source.getLines.toList
    lines.groupBy({line =>
      val split = "\t".r.split(line)
      val vnVerb = split(0)
      val vnSense = split(1).toInt
      val vnGloss = split(2)
      val wnWord = split(3)
      val wordParts = "_".r.split(wnWord)
      val preps = if (wordParts.size <= 1) { None } else { Some(wordParts.drop(1).toList) }
      VerbNetSense(vnVerb, vnSense, vnGloss, preps)
    }).mapValues({ sameVnLines =>
      sameVnLines.map({ sameVnLine =>
        val split = "\t".r.split(sameVnLine)
        val wnWord = split(3)
        val wnSense = split(4).toInt
        wordNetUtils.getWordSense(wnWord, POS.VERB, wnSense)
      }).toSet
    })
  }
  
  /**
   * Given a file with rows like:
   *   wn_word wn_sense vn_verb vn_sense vn_gloss
   *   
   * Get a map from WordNet senses to a set of VerbNet senses.
   */
  private def processWnToVn(): Map[IWord, Set[VerbNetSense]] = {
    val source = Source.fromFile(wnToVnPath)
    val lines = source.getLines.toList
    lines.groupBy({line =>
      val split = "\t".r.split(line)
      val wnWord = split(0)
      val wnSense = split(1).toInt
      wordNetUtils.getWordSense(wnWord, POS.VERB, wnSense)
    }).mapValues({ sameWnLines =>
      sameWnLines.map({ sameWnLine =>
        val split = "\t".r.split(sameWnLine)
        val wnWord = split(0)
        val vnVerb = split(2)
        val vnSense = split(3).toInt
        val vnGloss = split(4)
        val wordParts = "_".r.split(wnWord)
        val preps = if (wordParts.size <= 1) { None } else { Some(wordParts.drop(1).toList) }
        VerbNetSense(vnVerb, vnSense, vnGloss, preps)
      }).toSet
    })
  }
  
  private def writeWnToVn(writer: BufferedWriter) = {
    wnToVn.foreach({ case (wordNetSense, verbNetSenses) =>
      val gloss = wordNetUtils.getGloss(wordNetSense)
      val tagCount = wordNetUtils.getTagCount(wordNetSense)
      verbNetSenses.foreach({ verbNetSense =>
        val row = List(
          wordNetUtils.wordToString(wordNetSense, senseNumber=true),
          verbNetSense,
          wordNetUtils.getGloss(wordNetSense),
          verbNetSense.getGloss,
          wordNetUtils.getTagCount(wordNetSense)
        ).mkString("\t")
        writer.write(row)
        writer.newLine()
      })
    })
  }
  
  def generateEntailmentGraph(traceWriter: Option[BufferedWriter] = None): EntailmentGraph = {
    val entailmentGraph = new EntailmentGraph
    
    vnToWn.foreach({ case (verbNetSense, wordNetSenses) =>
      val synonyms = wordNetSenses.flatMap({ word =>
        wordNetUtils.getSynonyms(word).map({
          synonym => (word, synonym)
        })
      })
      val hyponyms = wordNetSenses.flatMap({ word =>
        wordNetUtils.getHyponyms(word).map({
          hyponym => (word, hyponym)
        })
      })
      
      synonyms.foreach({ case (word, synonym) =>
        wnToVn.get(synonym) match {
          case Some(vnSynonyms) => {
            vnSynonyms.foreach({ vnSynonym =>
              entailmentGraph.addSynonym(verbNetSense, vnSynonym);
              traceWriter match {
                case Some(writer) => {
                  val traceCol = List(
                    verbNetSense,
                    wordNetUtils.wordToString(word, tagCount=true),
                    wordNetUtils.wordToString(synonym, tagCount=true),
                    vnSynonym
                  ).mkString(" = ")
                  val row = List(
                    verbNetSense,
                    vnSynonym,
                    verbNetSense.getGloss,
                    vnSynonym.getGloss,
                    traceCol
                  ).mkString("\t")
                  writer.write(row)
                  writer.newLine()
                }
                case None =>
              }
            })
          }
          case None =>
        }
      })
      hyponyms.foreach({ case (word, hyponym) =>
        wnToVn.get(hyponym) match {
          case Some(vnHyponyms) => {
            vnHyponyms.foreach({ vnHyponym =>
              entailmentGraph.addEntailment(vnHyponym, verbNetSense);
              traceWriter match {
                case Some(writer) => {
                  val traceCol = List(
                    List(
                      vnHyponym,
                      wordNetUtils.wordToString(hyponym, tagCount=true)
                    ).mkString(" = "),
                    List(
                      wordNetUtils.wordToString(word, tagCount=true),
                      verbNetSense
                    ).mkString(" = ")
                  ).mkString(" => ")
                  val row = List(
                    vnHyponym,
                    verbNetSense,
                    vnHyponym.getGloss,
                    verbNetSense.getGloss,
                    traceCol
                  ).mkString("\t")
                  writer.write(row)
                  writer.newLine()
                }
                case None =>
              }
            })
          }
          case None =>
        }
      })
    })
    entailmentGraph
  }
}

object EntailmentGraphGenerator {
  def writeGraph(graph: EntailmentGraph, path: String): Unit = {
    val writer = new BufferedWriter(new FileWriter(path))
    def writeEdges(edges: Map[VerbNetSense, Set[VerbNetSense]], edgeName: String): Unit = {
      edges.foreach({ case(vn1, vn2s) =>
        vn2s.foreach({ vn2 =>
          val row = List(vn1, vn2, edgeName).mkString("\t")
          writer.write(row)
          writer.newLine()
        })
      })
    }
    writeEdges(graph.synonymEdges.toMap, "synonym")
    writeEdges(graph.hyponymEdges.toMap, "entailment")
    writer.flush()
    writer.close()
  }
  
  def main(args: Array[String]): Unit = {
    var inputDir = "."
    var outputDir = "."
    
    val parser = new OptionParser() {
      arg("inputDir", "Directory of input files.", {str => inputDir = str})
      arg("outputDir", "Directory of output files.", {str => outputDir = str})
    }
    if (!parser.parse(args)) return
    
    val wordNetPath = List(inputDir, "WordNet-3.0", "dict").mkString(File.separator)
    val vnToWnPath = List(inputDir, "vn-wn.tsv").mkString(File.separator)
    val wnToVnPath = List(inputDir, "wn-vn.tsv").mkString(File.separator)
    
    val traceWriterPath = List(outputDir, "vn_trace.txt").mkString(File.separator)
    val graphWriterPath = List(outputDir, "vn_to_vn.txt").mkString(File.separator)
    val wnToVnOutputPath = List(outputDir, "wn_to_vn.txt").mkString(File.separator)
    val traceWriter = new BufferedWriter(new FileWriter(traceWriterPath))
    val wnToVnWriter = new BufferedWriter(new FileWriter(wnToVnOutputPath))
    
    val generator = new EntailmentGraphGenerator(vnToWnPath, wnToVnPath, wordNetPath)
    val graph = generator.generateEntailmentGraph(traceWriter=Some(traceWriter))
    generator.writeWnToVn(wnToVnWriter)
    
    traceWriter.flush()
    traceWriter.close()
    wnToVnWriter.flush()
    wnToVnWriter.close()
    writeGraph(graph, graphWriterPath)
  }
}