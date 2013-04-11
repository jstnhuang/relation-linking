package edu.washington.cs.knowitall.relation;

import java.util.Set;

/**
 * Interface for a relation linker.
 */
public interface RelationLinker {
  public Set<String> getRelationLinks(String relationPhrase);
}
