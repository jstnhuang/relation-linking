package edu.washington.cs.knowitall.relation.linker

import scala.collection.Seq
import scala.collection.immutable.Set
import edu.knowitall.tool.postag.PostaggedToken
import edu.knowitall.tool.srl.ClearSrl
import edu.knowitall.tool.parse.ClearParser
import edu.washington.cs.knowitall.relation.RelationPhraseFinder

/**
 * Uses the Clear semantic role labeling system to link the head verb of the relation phrase.
 */
object SrlRelationLinker extends RelationLinker {
  val clearParser = new ClearParser()
  val clearSrl = new ClearSrl()
  
  def getRelationLinks(phrase: Seq[PostaggedToken], context: Option[Seq[PostaggedToken]]):
      Set[String] = {
    val headPhrase = RelationPhraseFinder.getHeadPhrase(phrase)
    val sentence = context match {
      case Some(sent) => sent
      case None => headPhrase
    }
    val text = sentence.map(_.string).mkString(" ")
    println(text)
    val graph = clearParser.dependencyGraph(text)
    println(graph)
    val frames = clearSrl(graph)
    println(frames)
    val frameIndex = frames.lastIndexWhere({ frame =>
      sentence.head.offsets.superset(frame.relation.node.tokenInterval)
    })
    if (frameIndex < 0) {
      Set.empty[String]
    } else {
      Set(frames(frameIndex).relation.toString())
    }
  }
}