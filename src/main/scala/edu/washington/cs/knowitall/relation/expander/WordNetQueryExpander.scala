package edu.washington.cs.knowitall.relation.expander

import edu.washington.cs.knowitall.relation.RelationPhraseFinder
import edu.washington.cs.knowitall.relation.experiment.BenchmarkQuery
import edu.washington.cs.knowitall.model.OpenIeQuery
import edu.washington.cs.knowitall.WordNetUtils
import edu.washington.cs.knowitall.relation.linker.WordNetRelationLinker
import edu.washington.cs.knowitall.model.QueryRel
import edu.washington.cs.knowitall.model.QueryArg
import edu.mit.jwi.item.IWord

/**
 * Expands the relation phrase of a query based on its WordNet sense.
 */
class WordNetQueryExpander(wordNetUtils: WordNetUtils) extends QueryExpander {
  val wordNetLinker = new WordNetRelationLinker(wordNetUtils)
  val MAX_NUM_SENSES = 3
  
  def getName(): String = "WordNet"

  def expandQuery(rawQuery: BenchmarkQuery): OpenIeQuery = {
    val queryArg1 = QueryArg.fromString(rawQuery.arg1.getOrElse(""))
    val queryRel = QueryRel.fromString(rawQuery.rel.getOrElse(""))
    val queryArg2 = QueryArg.fromString(rawQuery.arg2.getOrElse(""))
    val relString = queryRel.getFirstRel.get
    val (arg1Tags, relTags, arg2Tags) = QueryExpander.tagQuery(queryArg1, relString, queryArg2)
    
    wordNetLinker.getRelationLinks(relTags, maxNumSenses=100) match {
      case Some((preHeadWords, wordNetSenses, postHeadWords)) => {
        val preString = preHeadWords.mkString(" ")
        val postString = postHeadWords.mkString(" ")
        val entailingSenses = wordNetSenses.flatMap({ sense =>
          val tc = transitiveClosure(sense)
//        tc.foreach { closure => println(relString + "\t" + closure) }
          tc.map(_.lastElement)
        }).map({ word =>
          List(preString, word.getLemma().replace("_", " "), postString).mkString(" ").trim()
        })
        
        new OpenIeQuery(
          queryArg1,
          new QueryRel(rels=Some(entailingSenses)),
          queryArg2
        )
      }
      case None => {
        System.err.println("No WordNet senses for " + queryRel.getFirstRel.getOrElse("(None)"))
        null
      }
    }
  }
  
  def transitiveClosure(word: IWord): Set[TransitiveClosurePath] = {
    val queue = new scala.collection.mutable.Queue[TransitiveClosurePath]
    queue += new TransitiveClosurePath(word, "identity")
    var closure = Set.empty[TransitiveClosurePath]
    
    while (!queue.isEmpty) {
      val currentPath = queue.dequeue
      val lastElement = currentPath.lastElement()
      val hyponyms = wordNetUtils.getHyponyms(lastElement)
      val nextPaths = hyponyms
          .filter(!currentPath.containsElement(_))
          .map(currentPath.withElement(_, "hyponym"))
      
      closure = closure + currentPath
      queue ++= nextPaths
    }
    
    closure
  }
  
  class TransitiveClosurePath(path: Seq[IWord], elementTypes: Map[IWord, String]) {    
    def this(word: IWord, stepType: String) = {
      this(Seq(word), Map(word -> stepType))
    }
    
    def withElement(word: IWord, stepType: String) = {
      new TransitiveClosurePath(path :+ word, elementTypes + (word -> stepType))
    }
    
    def containsElement(word: IWord) = elementTypes.keySet.contains(word)
    
    def lastElement(): IWord = {
      path.last
    }
    
    override def toString(): String = {
      path.map({ word =>
        List(
          wordNetUtils.wordToString(word, false, false),
          wordNetUtils.getSenseNumber(word),
          wordNetUtils.getTagCount(word),
          elementTypes(word)
        )
      }).flatten.mkString("\t")
    }
  }
}
