package edu.washington.cs.knowitall.relation

import edu.washington.cs.knowitall.model.PropbankSense

object EntailmentGraphEdgeType extends Enumeration {
  type EdgeType = Value
  val Synonym, Hyponym = Value
}
import EntailmentGraphEdgeType._

/**
 * A directed graph where Propbank senses go to synonymous or entailed Propbank senses.
 */
class EntailmentGraph {
  var synonymEdges = Map[PropbankSense, Set[PropbankSense]]()
  var hyponymEdges = Map[PropbankSense, Set[PropbankSense]]()
  var nodes = Set[PropbankSense]()
  
  /**
   * Add an edge to the graph such that p1 and p2 are synonymous (bidirectional entailment).
   */
  def addSynonym(p1: PropbankSense, p2: PropbankSense) = {
    addEdge(p1, p2, Synonym)
    addEdge(p2, p1, Synonym)
  }
  
  /**
   * Add an edge to the graph such that p1 entails p2, i.e., p2 is a hyponym/troponym of p1.
   */
  def addEntailment(p1: PropbankSense, p2: PropbankSense) = {
    addEdge(p1, p2, Hyponym)
  }
  
  /**
   * Add an edge saying that p1 entails p2.
   */
  private def addEdge(p1: PropbankSense, p2: PropbankSense, edgeType: EdgeType): Unit = {
    edgeType match {
      case Synonym => {
        synonymEdges = synonymEdges.get(p1) match {
          case Some(senses) => synonymEdges + (p1 -> (senses+p2))
          case None => synonymEdges + (p1 -> Set(p2))
        }
      }
      case Hyponym => {
        hyponymEdges = hyponymEdges.get(p1) match {
          case Some(senses) => hyponymEdges + (p1 -> (senses+p2))
          case None => hyponymEdges + (p1 -> Set(p2))
        }
      }
    }
    
    nodes = nodes + p1 + p2
  }
}