package edu.washington.cs.knowitall.relation

import edu.knowitall.tool.postag.PostaggedToken

/**
 * Gets the head phrase of a relation string. We define this as the last verb plus all following
 * words (usually prepositions). 
 */
object RelationPhraseFinder {
  def getHeadPhrase(relTags: Seq[PostaggedToken]): (Seq[PostaggedToken], Integer) = {
    val index = relTags.lastIndexWhere(token => token.isVerb)
    if (index > 0) {
      (relTags.drop(index), index)
    } else {
      (Seq.empty[PostaggedToken], index)
    }
  }
}
