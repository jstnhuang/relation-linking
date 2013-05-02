package edu.washington.cs.knowitall.relation.linker

import edu.knowitall.tool.postag.PostaggedToken

/**
 * Trait for a relation linker.
 */
trait RelationLinker {
  /**
   * Do relation linking on the given verb phrase, with the optional context. All relation links are
   * expressed as strings for now. WordNet looks like "throw_up#1", SRL looks like "throw.01" and
   * VerbNet looks like "throw-1_up".
   */
  def getRelationLinks(phrase: Seq[PostaggedToken], context: Option[Seq[PostaggedToken]]=None):
    Set[String]
}