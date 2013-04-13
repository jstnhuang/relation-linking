package edu.washington.cs.knowitall

import edu.washington.cs.knowitall.browser.extraction.ExtractionGroup
import edu.washington.cs.knowitall.browser.extraction.ReVerbExtraction

trait QueryExecutor {
  def execute(queryString: String): Iterator[ExtractionGroup[ReVerbExtraction]]
}