package edu.washington.cs.knowitall.relation

import java.io.File

object Constants {
  def wordNetPath(baseDir: String): String = {
    List(baseDir, "WordNet-3.0", "dict").mkString(File.separator)
  }
  def verbNetDbPath(baseDir: String): String = {
    List("localhost:1527", baseDir, "vntables").mkString(File.separator)
  }
}