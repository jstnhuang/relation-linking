package edu.washington.cs.knowitall.relation.experiment

/**
 * Basic wrapper around the experiment output (all strings for now).
 */
class RelationInferenceResult(systemName: String, testQuery: String, expandedQuery: String,
    tuple: String, tag: String, sentence: String, tupleLinks: String) {
  override def toString(): String = {
    "%s\t%s\t%s\t%s\t%s\t%s\t%s".format(
      systemName, testQuery, expandedQuery, tuple, tag, sentence, tupleLinks
    ).filterNot(_ == '"')
  }

  def getTagKey() = (testQuery, tuple, sentence)
  def getTag() = tag
}

object RelationInferenceResult {
  def fromString(line: String): RelationInferenceResult = {
    val columns = "\t".r.split(line).map(_.trim())
    new RelationInferenceResult(columns(0), columns(1), columns(2), columns(3), columns(4),
      columns(5), columns(6))
  }
}
