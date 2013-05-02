package edu.washington.cs.knowitall.relation

import edu.washington.cs.knowitall.model.{OpenIeQuery, QueryArg, QueryRel}
import edu.washington.cs.knowitall.relation.experiment.BenchmarkQuery
import edu.knowitall.tool.postag.PostaggedToken
import edu.knowitall.tool.postag.OpenNlpPostagger
import edu.knowitall.tool.tokenize.OpenNlpTokenizer
import edu.knowitall.tool.tokenize.Token

trait QueryExpander {
  def getName(): String
  def expandQuery(rawQuery: BenchmarkQuery): OpenIeQuery
  val tokenizer = new OpenNlpTokenizer()
  val tagger = new OpenNlpPostagger()
  
  def tokenizeQuery(arg1: String, rel: String, arg2: String):
      (Seq[Token], Seq[Token], Seq[Token]) = (
    tokenizer.tokenize(arg1),
    tokenizer.tokenize(rel),
    tokenizer.tokenize(arg2)
  )
  
  /**
   * Gets a textual phrase for the given arg string. The text comes from either the arg string
   * itself, the entity, or the type. If none of those are set, then the result is "what".
   */
  def getArgText(arg: QueryArg): String = {
    arg.arg match {
      case Some(argString) => argString
      case None => arg.entity match {
        case Some(entity) => entity.replace("_", " ")
        case None => arg.types match {
          case Some(types) => types.replace("_", " ")
          case None => "what"
        }
      }
    }
  }
  
  /**
   * Attempts to get the part of speech tags for a query. If the relation phrase doesn't exist, then
   * it just tags the arguments as normal. If the relation phrase does exist, then it replaces any
   * missing arguments with the word "what," and tags the resulting phrase.
   */
  def tagQuery(arg1: QueryArg, rel: QueryRel, arg2: QueryArg):
      (Seq[PostaggedToken], Seq[PostaggedToken], Seq[PostaggedToken]) = {
    rel.rel match {
      case Some(relString) => {
        val arg1Text = getArgText(arg1)
        val arg2Text = getArgText(arg2)
        val (arg1Tokens, relTokens, arg2Tokens) = tokenizeQuery(
          arg1Text,
          relString,
          arg2Text
        )
        val arg1End = arg1Tokens.size
        val arg2End = arg1End + relTokens.size
        
        val sentence = arg1Tokens ++ relTokens ++ arg2Tokens
        val taggedTokens = tagger.postagTokens(sentence)
        val result = (
          taggedTokens.take(arg1End),
          taggedTokens.slice(arg1End, arg2End),
          taggedTokens.drop(arg2End)
        )
        result
      }
      case None => {
        (tagger.postag(arg1.arg.getOrElse("")), List(), tagger.postag(arg2.arg.getOrElse("")))
      }
    }
  }
}