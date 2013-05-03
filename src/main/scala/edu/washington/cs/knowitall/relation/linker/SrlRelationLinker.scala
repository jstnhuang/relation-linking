package edu.washington.cs.knowitall.relation.linker

import scala.collection.Seq
import scala.collection.immutable.Set
import edu.knowitall.tool.postag.PostaggedToken
import edu.knowitall.tool.srl.ClearSrl
import edu.knowitall.tool.parse.ClearParser
import edu.washington.cs.knowitall.relation.RelationPhraseFinder
import edu.knowitall.collection.immutable.Interval

/**
 * Uses the Clear semantic role labeling system to link the head verb of the relation phrase.
 */
object SrlRelationLinker extends RelationLinker {
  val clearParser = new ClearParser()
  val clearSrl = new ClearSrl()
  
  def getRelationLinks(
      phrase: Seq[PostaggedToken],
      context: Option[(Seq[PostaggedToken], Interval)]): Set[String] = {
    val headPhrase = RelationPhraseFinder.getHeadPhrase(phrase)
    val (sentence, interval) = context match {
      case Some((sent, interval)) => (sent, interval)
      case None => (
        headPhrase,
        Interval.span(headPhrase.map(_.interval))
      )
    }
    val text = sentence.map(_.string).mkString(" ")
    val graph = clearParser.dependencyGraph(text)
    val frames = clearSrl(graph)
    println(sentence + " ------- " + interval)
    val frameIndex = frames.lastIndexWhere({ frame =>
      println(frame.relation.node.text + ": " + frame.relation.node.tokenInterval)
      interval.superset(frame.relation.node.tokenInterval)
    })
    if (frameIndex < 0) {
      Set.empty[String]
    } else {
      Set(frames(frameIndex).relation.toString())
    }
  }
}