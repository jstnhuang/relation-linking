package edu.washington.cs.knowitall.relation

import java.io.BufferedWriter
import java.io.File
import scala.io.Source
import edu.mit.jwi.item.IWord
import edu.mit.jwi.item.POS
import edu.washington.cs.knowitall.WordNetUtils
import edu.washington.cs.knowitall.model.PropbankSense
import scopt.OptionParser
import java.io.FileWriter

class EntailmentGraphGenerator(pbToWnPath: String, wnToPbPath: String, wordNetPath: String) {
  val wordNetUtils = new WordNetUtils(wordNetPath)
  val pbToWn = processPbToWn()
  val wnToPb = processWnToPb()
  
  /**
   * Given a file with rows like:
   *   elaborate.01 elaborate   1
   *   elaborate.01 elaborate   4
   *   
   * Get a map from Propbank senses to a set of WordNet senses.
   */
  private def processPbToWn(): Map[PropbankSense, Set[IWord]] = {
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
  private def processWnToPb(): Map[IWord, Set[PropbankSense]] = {
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
  
  def generateEntailmentGraph(traceWriter: Option[BufferedWriter] = None): EntailmentGraph = {
    val entailmentGraph = new EntailmentGraph
    
    pbToWn.foreach({ case (propbankSense, wordNetSenses) =>
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
        wnToPb.get(synonym) match {
          case Some(pbSynonyms) => {
            pbSynonyms.foreach({ pbSynonym =>
              entailmentGraph.addSynonym(propbankSense, pbSynonym);
              traceWriter match {
                case Some(writer) => {
                  val traceCol = List(
                    propbankSense,
                    wordNetUtils.wordToString(word, tagCount=true),
                    wordNetUtils.wordToString(synonym, tagCount=true),
                    pbSynonym
                  ).mkString(" = ")
                  val row = List(propbankSense, pbSynonym, traceCol).mkString("\t")
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
        wnToPb.get(hyponym) match {
          case Some(pbHyponyms) => {
            pbHyponyms.foreach({ pbHyponym =>
              entailmentGraph.addEntailment(propbankSense, pbHyponym);
              traceWriter match {
                case Some(writer) => {
                  val traceCol = List(
                    List(
                      propbankSense,
                      wordNetUtils.wordToString(word, tagCount=true)
                    ).mkString(" = "),
                    List(
                      wordNetUtils.wordToString(hyponym, tagCount=true),
                      pbHyponym
                    ).mkString(" = ")
                  ).mkString(" => ")
                  val row = List(propbankSense, pbHyponym, traceCol).mkString("\t")
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
    def writeEdges(edges: Map[PropbankSense, Set[PropbankSense]], edgeName: String): Unit = {
      edges.foreach({ case(pb1, pb2s) =>
        pb2s.foreach({ pb2 =>
          val row = List(pb1, pb2, edgeName).mkString("\t")
          writer.write(row)
          writer.newLine()
        })
      })
    }
    writeEdges(graph.synonymEdges, "synonym")
    writeEdges(graph.hyponymEdges, "entailment")
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
    val pbToWnPath = List(inputDir, "pb-wn.tsv").mkString(File.separator)
    val wnToPbPath = List(inputDir, "wn-pb.tsv").mkString(File.separator)
    
    val traceWriterPath = List(outputDir, "pb_trace.txt").mkString(File.separator)
    val graphWriterPath = List(outputDir, "pb_to_pb.txt").mkString(File.separator)
    val traceWriter = new BufferedWriter(new FileWriter(traceWriterPath))
    
    val generator = new EntailmentGraphGenerator(pbToWnPath, wnToPbPath, wordNetPath)
    val graph = generator.generateEntailmentGraph(traceWriter=Some(traceWriter))
    
    traceWriter.flush()
    traceWriter.close()
    writeGraph(graph, graphWriterPath)
  }
}