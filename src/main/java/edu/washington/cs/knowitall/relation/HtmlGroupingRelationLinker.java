package edu.washington.cs.knowitall.relation;

import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.HashSet;
import java.util.Set;

public class HtmlGroupingRelationLinker implements RelationLinker {
  Connection connection = null;
  private static final String derbyDriver = "org.apache.derby.jdbc.EmbeddedDriver";
  
  public HtmlGroupingRelationLinker(String basePath) {
    String connectionUrl = "jdbc:derby:" + basePath + "pbtables";
    try {
      Class.forName(derbyDriver);
      connection = DriverManager.getConnection(connectionUrl);
    } catch (SQLException e) {
      throw new RuntimeException("Could not open Derby DB at " + connectionUrl, e);
    } catch (ClassNotFoundException e) {
      throw new RuntimeException("Could not find Derby driver " + derbyDriver, e);
    }
  }
  
  @Override
  public Set<String> getRelationLinks(String relationPhrase) {
    Set<String> relationLinks = new HashSet<String>();
    PreparedStatement selectStatement = null;
    try {
      selectStatement = connection.prepareStatement(
        "SELECT pb, prob, count FROM str_to_pb_syns WHERE vp=? order by prob desc fetch first 10 rows only"
      );
      selectStatement.setString(1, relationPhrase);
      ResultSet resultSet = selectStatement.executeQuery();
      while (resultSet.next()) {
        String propBankSense = resultSet.getString(1);
        double cprob = resultSet.getDouble(2);
        int count = resultSet.getInt(3);
        relationLinks.add(propBankSense);
      }
      selectStatement.close();
    } catch (SQLException e) {
      throw new RuntimeException("Error with pbtable lookup in getRelationLinks.", e);
    }
    return relationLinks;
  }
}
