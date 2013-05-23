package edu.washington.cs.knowitall.relation

import scala.collection.mutable.Map

import edu.washington.cs.knowitall.model.VerbNetSense

object EntailmentGraphEdgeType extends Enumeration {
  type EdgeType = Value
  val Synonym, Hyponym = Value
}
import EntailmentGraphEdgeType._

/**
 * A directed graph where VerbNetSense senses go to synonymous or entailed VerbNetSense senses.
 */
class EntailmentGraph {
  var synonymEdges = Map[VerbNetSense, Set[VerbNetSense]]()
  var hyponymEdges = Map[VerbNetSense, Set[VerbNetSense]]()
  var nodes = Set[VerbNetSense]()
  
  /**
   * Add an edge to the graph such that p1 and p2 are synonymous (bidirectional entailment).
   */
  def addSynonym(v1: VerbNetSense, v2: VerbNetSense) = {
    addEdge(v1, v2, Synonym)
    addEdge(v2, v1, Synonym)
  }
  
  /**
   * Add an edge to the graph such that p1 entails p2, i.e., p2 is a hyponym/troponym of p1.
   */
  def addEntailment(v1: VerbNetSense, v2: VerbNetSense) = {
    addEdge(v1, v2, Hyponym)
  }
  
  /**
   * Add an edge saying that p1 entails p2.
   */
  private def addEdge(v1: VerbNetSense, v2: VerbNetSense, edgeType: EdgeType): Unit = {
    edgeType match {
      case Synonym => {
        synonymEdges.get(v1) match {
          case Some(senses) => synonymEdges(v1) = senses + v2
          case None => synonymEdges(v1) = Set(v2)
        }
      }
      case Hyponym => {
        hyponymEdges.get(v1) match {
          case Some(senses) => hyponymEdges(v1) = senses + v2
          case None => hyponymEdges(v1) = Set(v2)
        }
      }
    }
    
    nodes = nodes + v1 + v2
  }
}