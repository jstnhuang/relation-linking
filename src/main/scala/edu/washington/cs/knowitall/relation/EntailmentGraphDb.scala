package edu.washington.cs.knowitall.relation

import edu.washington.cs.knowitall.db.DerbyHandler

class EntailmentGraphDb(derbyHandler: DerbyHandler) {
  def verbNetSensesFromWordNet(wordNetSense: String): Set[String] = {
    var verbNetSenses = Set.empty[String]
    val statement = derbyHandler.prepareStatement(
      "SELECT vn FROM wn_to_vn WHERE wn=?"
    )
    statement.setString(1, wordNetSense)
    val results = derbyHandler.query(statement)
    while (results.next()) {
      val verbNetSense = results.getString(1)
      verbNetSenses += verbNetSense
    }
    verbNetSenses
  }
  
  def entailedVerbNetSenses(verbNetSense: String): Set[String] = {
    var entailedSenses = Set.empty[String]
    val statement = derbyHandler.prepareStatement(
      "SELECT vn2 FROM vn_to_vn WHERE vn1=?"
    )
    statement.setString(1, verbNetSense)
    val results = derbyHandler.query(statement)
    while (results.next()) {
      val entailedSense = results.getString(1)
      entailedSenses += entailedSense
    }
    entailedSenses
  }
  
  def entailingVerbNetSenses(verbNetSense: String): Set[String] = {
    var entailingSenses = Set.empty[String]
    val statement = derbyHandler.prepareStatement(
      "SELECT vn1 FROM vn_to_vn WHERE vn2=?"
    )
    statement.setString(1, verbNetSense)
    val results = derbyHandler.query(statement)
    while (results.next()) {
      val entailingSense = results.getString(1)
      entailingSenses += entailingSense
    }
    entailingSenses
  }
}

