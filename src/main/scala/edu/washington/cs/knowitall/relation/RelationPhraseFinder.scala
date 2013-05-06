package edu.washington.cs.knowitall.relation

import edu.knowitall.tool.postag.PostaggedToken

/**
 * Gets the head phrase of a relation string. We define this as the last verb plus all following
 * words (usually prepositions). 
 */
object RelationPhraseFinder {
  def getHeadPhrase(relTags: Seq[PostaggedToken]): Seq[PostaggedToken] = {
    val index = relTags.lastIndexWhere(token => token.isVerb)
    relTags.drop(index);
  }
  def getPrepositions(relTags: Seq[PostaggedToken]): Seq[PostaggedToken] = {
    val lastVerbIndex = relTags.lastIndexWhere(token => token.isVerb)
    relTags.drop(lastVerbIndex + 1).filter(_.isPreposition)
  }
}
