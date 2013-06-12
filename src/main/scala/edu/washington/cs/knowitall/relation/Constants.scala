package edu.washington.cs.knowitall.relation

import java.io.File

object Constants {
  def wordNetPath(baseDir: File): File = {
    new File(new File(baseDir, "WordNet-3.0"), "dict")
  }
//  def relationLinkingDbPath(baseDir: String): String = {
//    List("localhost:1527", baseDir, "relationlinking").mkString(File.separator)
//  }
  def derbyDbUrl(dbPath: File) = {
    "localhost:1527/%s/".format(dbPath.getCanonicalPath())
  }
}