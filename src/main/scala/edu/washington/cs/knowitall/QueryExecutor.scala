package edu.washington.cs.knowitall

import edu.knowitall.openie.models.ExtractionGroup
import edu.knowitall.openie.models.ReVerbExtraction

trait QueryExecutor {
  def execute(queryString: String): Iterator[ExtractionGroup[ReVerbExtraction]]
}
