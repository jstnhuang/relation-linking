package edu.washington.cs.knowitall.relation.linker

import edu.knowitall.collection.immutable.Interval
import edu.knowitall.tool.postag.PostaggedToken
import edu.washington.cs.knowitall.WordNetUtils
import edu.washington.cs.knowitall.db.DerbyHandler

/**
 * Links a phrase to a set of VerbNet senses. It first uses the WordNet linker to get the WordNet
 * sense. Then it uses the WordNet-to-VerbNet table to get the VerbNet senses. To use this class,
 * you must have the Derby tables in basePath (the db must be named the same as Constants.VNTABLES).
 * You also need to have WordNet in basePath.
 */
class VerbNetRelationLinker(verbNetDbPath: String, wordNetUtils: WordNetUtils)
    extends RelationLinker {
  val derbyHandler = new DerbyHandler(verbNetDbPath)
  val wordNetLinker = new WordNetRelationLinker(wordNetUtils)
  
  /**
   * Gets the VerbNet senses associated with the given phrase. It uses the WordNet linker to link
   * the phrase to a WordNet sense. Then, it does a lookup of that WordNet sense in the WordNet to
   * VerbNet table to get the VerbNet senses. If no senses are found, then it returns the empty set.
   */
  def getRelationLinks(
      phrase: Seq[PostaggedToken],
      context: Option[(Seq[PostaggedToken], Interval)] = None): Set[String] = {
    val wordNetSenses = wordNetLinker.getRelationLinks(phrase)
    
    // TODO: get synonyms, fall back on hypernyms
    
    var relationLinks = Set[String]()
    if (wordNetSenses.size == 0) {
      relationLinks
    } else {
      val selectStatement = derbyHandler.prepareStatement(
        "SELECT vn FROM wn_to_vn WHERE wn IN ("
        + wordNetSenses.toSeq.map(_ => "?").mkString(", ") + ")"
      )
      var index = 1;
      wordNetSenses.foreach({ wordNetSense =>
        selectStatement.setString(index, wordNetSense)
        index += 1
      })
      
      val results = derbyHandler.query(selectStatement);
      while(results.next()) {
        val verbNetSense = results.getString(1)
        relationLinks += verbNetSense
      }
      selectStatement.close()
      relationLinks
    }
  }
}