package edu.washington.cs.knowitall.db

import java.sql.DriverManager
import java.sql.SQLException
import java.sql.Connection
import java.sql.ResultSet
import java.sql.PreparedStatement

/**
 * Light wrapper around a Derby DB connection.
 */
class DerbyHandler(dbPath: String) {
  val connectionUrl = "jdbc:derby:" + dbPath;
  var connection: Connection = null;
  try {
    Class.forName(DerbyConstants.DERBY_DRIVER).newInstance();
    connection = DriverManager.getConnection(connectionUrl)
  } catch {
    case e: SQLException => throw new RuntimeException(
      "Could not open Derby DB at " + connectionUrl, e)
    case e: ClassNotFoundException => throw new RuntimeException(
      "Could not find Derby driver " + DerbyConstants.DERBY_DRIVER, e)
  }
  
  /**
   * Gets the prepared statement for this connection. The user has to set the wildcards, then call
   * {@link query}.
   */
  def prepareStatement(queryString: String): PreparedStatement = {
    connection.prepareStatement(queryString)
  }
  
  /**
   * Executes the given prepared statement. Wildcards must be set at this point.
   */
  def query(statement: PreparedStatement): ResultSet = {
    try {
      statement.executeQuery()
    } catch {
      case e: SQLException => throw new RuntimeException("Error with Derby lookup: " + statement, e)
    }
  }
}

object DerbyConstants {
  val DERBY_DRIVER = "org.apache.derby.jdbc.EmbeddedDriver"
}