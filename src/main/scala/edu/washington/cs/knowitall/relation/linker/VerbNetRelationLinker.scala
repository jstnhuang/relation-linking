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
    val wordNetSenses = wordNetLinker.getWordRelationLinks(phrase)
    
    val synonyms = wordNetSenses.flatMap(wordNetUtils.getSynonyms(_))
    val hypernyms = wordNetSenses.flatMap(wordNetUtils.getHypernyms(_))
    val senses = (synonyms ++ hypernyms).map(wordNetUtils.wordToString(_))
    
    var relationLinks = Set[String]()
    if (senses.size == 0) {
      relationLinks
    } else {
      val selectStatement = derbyHandler.prepareStatement(
        "SELECT vn FROM wn_to_vn WHERE wn IN ("
        + senses.toSeq.map(_ => "?").mkString(", ") + ")"
      )
      var index = 1;
      senses.foreach({ sense =>
        selectStatement.setString(index, sense)
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
