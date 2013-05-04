package edu.washington.cs.knowitall.relation.experiment

import edu.knowitall.tool.postag.OpenNlpPostagger

case class BenchmarkQuery(arg1: Option[String], rel: Option[String], arg2: Option[String]) {
  override def toString(): String = {
    "(%s, %s, %s)".format(arg1.getOrElse("*"), rel.getOrElse("*"), arg2.getOrElse("*"))
  }
}

object BenchmarkQuery {
  def fromLine(line: String): BenchmarkQuery = {
    val columns = "\t".r.split(line).map(_.trim())
    val arg1 = if(columns(0) != "") { Some(columns(0)) } else { None }
    val rel = if(columns(1) != "") { Some(columns(1)) } else { None }
    val arg2 = if(columns(2) != "") { Some(columns(2)) } else { None }
    BenchmarkQuery(arg1, rel, arg2)
  }
}