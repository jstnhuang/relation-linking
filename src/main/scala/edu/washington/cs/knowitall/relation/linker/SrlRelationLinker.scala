package edu.washington.cs.knowitall.relation.linker

import scala.collection.Seq
import scala.collection.immutable.Set
import edu.knowitall.tool.postag.PostaggedToken
import edu.knowitall.tool.srl.ClearSrl
import edu.knowitall.tool.parse.ClearParser
import edu.washington.cs.knowitall.relation.RelationPhraseFinder
import edu.knowitall.collection.immutable.Interval
import edu.washington.cs.knowitall.relation.PhraseNormalizer

/**
 * Uses the Clear semantic role labeling system to link the head verb of the relation phrase.
 */
object SrlRelationLinker {
  val clearParser = new ClearParser()
  val clearSrl = new ClearSrl()
  
  def getRelationLinks(
      phrase: Seq[PostaggedToken],
      context: Option[(Seq[PostaggedToken], Interval)]):
      Option[(Seq[String], Set[String], Seq[String])] = {
    val (headPhrase, headIndex) = RelationPhraseFinder.getHeadPhrase(phrase)
    val words = phrase.map(_.string).map(PhraseNormalizer.normalize(_))
    val preHeadWords = words.take(headIndex)
    val postHeadWords = words.drop(headIndex)
    
    val (sentence, interval) = context match {
      case Some((sent, interval)) => (sent, interval)
      case None => (
        headPhrase,
        Interval.span(headPhrase.map(_.interval))
      )
    }
    if (headPhrase.isEmpty) {
      None
    } else {
      val firstVerb = headPhrase(0).string
      val text = sentence.map(_.string).mkString(" ")
      val graph = clearParser.dependencyGraph(text)
      val frames = clearSrl(graph)
      val frameIndex = frames.lastIndexWhere({ frame =>
        firstVerb == frame.relation.node.text
      })
      if (frameIndex < 0) {
        None
      } else {
        Some((preHeadWords, Set(frames(frameIndex).relation.toString()), postHeadWords))
      }
    }
  }
}
