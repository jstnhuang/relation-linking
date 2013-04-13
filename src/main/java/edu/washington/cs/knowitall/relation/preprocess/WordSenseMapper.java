package edu.washington.cs.knowitall.relation.preprocess;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TreeSet;

import edu.mit.jwi.Dictionary;
import edu.mit.jwi.IDictionary;
import edu.mit.jwi.data.parse.SenseKeyParser;
import edu.mit.jwi.item.IIndexWord;
import edu.mit.jwi.item.ISenseEntry;
import edu.mit.jwi.item.ISenseKey;
import edu.mit.jwi.item.ISynset;
import edu.mit.jwi.item.ISynsetID;
import edu.mit.jwi.item.IWord;
import edu.mit.jwi.item.IWordID;
import edu.mit.jwi.item.POS;
import edu.mit.jwi.item.Pointer;
import edu.mit.jwi.item.SenseKey;
import edu.mit.jwi.item.SynsetID;

/**
 * Very quick and dirty code!
 * 
 * Builds a mapping from PropBank rolesets (which we refer to sometimes as PropBank senses) to
 * WordNet synonym sets and vice versa. To do this, we need two data sources:
 * <ul>
 *  <li>A mapping from PropBank senses to WordNet sense IDs (provided by VerbNet)</li>
 *  <li>WordNet itself</li>
 * </ul>
 * Outputs a flat, tab-separated file suitable for human examination or database import.
 */
public class WordSenseMapper {
  IDictionary wordNet;
  String inputDir;
  String outputDir;
  /** Maps PropBank senses to sets of WordNet sense keys. */
  Map<String, Set<ISenseKey>> senseIds;
  /** Maps WordNet synset IDs (from the sense keys of senseIds) to PropBank senses. */
  Map<ISynsetID, Set<String>> senseIdsInverse;
  
  public WordSenseMapper(String inputPath, String outputPath) {
    this.inputDir = inputPath;
    this.outputDir = outputPath;
    
    String wordNetPath = inputPath + "WordNet-3.0/dict/";
    try {
      wordNet = new Dictionary(new File(wordNetPath));
      wordNet.open();
    } catch (Exception e) {
      throw new RuntimeException(e);
    }
  }
  
  /**
   * Read the PropBank senses -> WordNet senses mapping from file.
   * 
   * Assumes the map is a tab-separated file with columns: verb, PropBank senses, VerbNet group,
   * WordNet senses. The PropBank senses and WordNet senses are separated by spaces. Also, assumes
   * the WordNet senses are missing the last two colon-separated fields regarding the head word, so
   * we just add "::" to the end of each.
   */
  private void readPropBankToWordNetMapping() {
    String pbToWnPath = inputDir + "VN-WN.txt";
    
    senseIds = new HashMap<String, Set<ISenseKey>>();
    senseIdsInverse = new HashMap<ISynsetID, Set<String>>();
    try {
      BufferedReader mapReader = new BufferedReader(new FileReader(pbToWnPath));
      mapReader.readLine();
      while(mapReader.ready()) {
        String line = mapReader.readLine();
        String[] lineParts = line.split("\t");
        String propBankColumn = lineParts[1].trim();
        String wordNetColumn = lineParts[3].trim();
        for (String propBankSense : propBankColumn.split(" ")) {
          for (String wordNetString : wordNetColumn.split(" ")) {
            if (wordNetString.startsWith("?")) {
              wordNetString = wordNetString.substring(1);
            }
            
            ISenseKey senseKey = SenseKeyParser.getInstance().parseLine(wordNetString + "::");
            if (senseIds.containsKey(propBankSense)) {
              senseIds.get(propBankSense).add(senseKey);
            } else {
              Set<ISenseKey> senseKeys = new HashSet<ISenseKey>();
              senseKeys.add(senseKey);
              senseIds.put(propBankSense, senseKeys);
            }
            
            ISenseEntry senseEntry = wordNet.getSenseEntry(senseKey);
            ISynsetID synsetId = new SynsetID(senseEntry.getOffset(), senseEntry.getPOS());
            if (senseIdsInverse.containsKey(synsetId)) {
              senseIdsInverse.get(synsetId).add(propBankSense);
            } else {
              Set<String> propBankSenses = new HashSet<String>();
              propBankSenses.add(propBankSense);
              senseIdsInverse.put(synsetId, propBankSenses);
            }
          }
        }
      }
      mapReader.close();
    } catch (IOException e) {
      throw new RuntimeException("Error reading mapping file.", e);
    }
  }
  
  /**
   * Given the PropBank sense to WordNet sense mapping, expand the synonyms from the WordNet sense
   * and add each to the synonym map, along with its count. This gives us a map from PropBank senses
   * to terms, as well as the count of a term given the PropBank sense.
   */
  private Map<String, Map<String, Integer>> makeSynonymMap() {
    Map<String, Map<String, Integer>> synonymMap = new HashMap<String, Map<String, Integer>>();
    for (String propBankSense : senseIds.keySet()) {
      Map<String, Integer> wordCounts = new HashMap<String, Integer>();
      if (synonymMap.containsKey(propBankSense)) {
        wordCounts = synonymMap.get(propBankSense);
      }
      Set<ISenseKey> senseKeys = senseIds.get(propBankSense);
      for (ISenseKey senseKey : senseKeys) {
        ISenseEntry senseEntry = wordNet.getSenseEntry(senseKey);
        ISynsetID id = new SynsetID(senseEntry.getOffset(), senseEntry.getPOS());
        ISynset synset = wordNet.getSynset(id);
        for (IWord word : synset.getWords()) {
          String lemma = word.getLemma();
          int wordCount = 0;
          if (wordCounts.containsKey(lemma)) {
            wordCount = wordCounts.get(lemma);
          }
          wordCounts.put(lemma, wordCount + senseEntry.getTagCount());
        }
      }
      synonymMap.put(propBankSense, wordCounts);
    }
    return synonymMap;
  }
  
  /**
   * Creates the inverse map of what {@link makeSynonymMap} gives us. This map goes from terms to
   * PropBank senses, and gives the count of a PropBank sense given the term.  
   */
  private Map<String, Map<String, Integer>> makeInverseMap(Map<String, Map<String, Integer>> synonymMap) {
    Map<String, Map<String, Integer>> inverseMap = new HashMap<String, Map<String, Integer>>();
    for (String propBankSense : synonymMap.keySet()) {
      Map<String, Integer> wordCounts = synonymMap.get(propBankSense);
      for (String word : wordCounts.keySet()) {
        Map<String, Integer> propBankCounts = new HashMap<String, Integer>();
        if (inverseMap.containsKey(word)) {
          propBankCounts = inverseMap.get(word);
        }
        int count = 0;
        if (propBankCounts.containsKey(propBankSense)) {
          count = propBankCounts.get(propBankSense);
        }
        propBankCounts.put(propBankSense, count + wordCounts.get(word));
        inverseMap.put(word, propBankCounts);
      }
    }
    return inverseMap;
  }
  
  /**
   * Makes a table where the columns are: PropBank sense, WordNet sense key, WordNet sense
   * frequency, term, term frequency (columns bundled together in SenseTableEntry).
   * 
   * The term frequency is, in fact, just the same as the frequency of the WordNet sense with the
   * same offset, but different term. For example, "anticipate" and "expect" both have the same
   * WordNet sense with offset 00721658, but there are two different sense entries for that offset:
   * expect%2:31:00:: and anticipate%2:31:00::. Those sense entries have different frequencies - 204
   * and 8, respectively. Technically this information is redundant, because it is elsewhere in the
   * table, but it may prove convenient anyway. 
   * 
   * @param senseIds
   * @return
   */
  private List<SenseTableEntry> makeSenseTable() {
    List<SenseTableEntry> senseTable = new ArrayList<SenseTableEntry>();
    for (String propBankSense : senseIds.keySet()) {
      for (ISenseKey senseKey : senseIds.get(propBankSense)) {
        ISenseEntry senseEntry = wordNet.getSenseEntry(senseKey);
        ISynsetID synsetId = new SynsetID(senseEntry.getOffset(), senseEntry.getPOS());
        ISynset synset = wordNet.getSynset(synsetId);
        for (IWord word : synset.getWords()) {
          ISenseKey termSenseKey = new SenseKey(word.getLemma(), word.getLexicalID(), synset);
          ISenseEntry termSenseEntry = wordNet.getSenseEntry(termSenseKey);
          int termFrequency = termSenseEntry.getTagCount();
          senseTable.add(new SenseTableEntry(
            propBankSense, senseKey, senseEntry.getTagCount(), word.getLemma(), termFrequency,
            termSenseKey
          ));
        }
      }
    }
    return senseTable;
  }
  
  /**
   * Wrapper class for entries in the sense table.
   */
  class SenseTableEntry {
    String propBankSense;
    ISenseKey senseKey;
    int senseFrequency;
    String term;
    int termFrequency;
    ISenseKey termSenseKey;
    
    public SenseTableEntry(String propBankSense, ISenseKey senseKey, int senseFrequency,
        String term, int termFrequency, ISenseKey termSenseKey) {
      super();
      this.propBankSense = propBankSense;
      this.senseKey = senseKey;
      this.senseFrequency = senseFrequency;
      this.term = term;
      this.termFrequency = termFrequency;
      this.termSenseKey = termSenseKey;
    }
    public String getPropBankSense() {
      return propBankSense;
    }
    public ISenseKey getSenseKey() {
      return senseKey;
    }
    public int getSenseFrequency() {
      return senseFrequency;
    }
    public String getTerm() {
      return term;
    }
    public int getTermFrequency() {
      return termFrequency;
    }
    public ISenseKey getTermSenseKey() {
      return termSenseKey;
    }
    @Override
    public String toString() {
      StringBuilder builder = new StringBuilder();
      builder.append("SenseTableEntry [propBankSense=").append(propBankSense).append(", senseKey=")
          .append(senseKey).append(", senseFrequency=").append(senseFrequency).append(", term=")
          .append(term).append(", termFrequency=").append(termFrequency).append(", termSenseKey=")
          .append(termSenseKey).append("]");
      return builder.toString();
    }
  }
  
  /**
   * Makes a table where the columns are: PropBank sense, WordNet sense key, WordNet sense
   * frequency, troponym, troponym sense frequency.
   * 
   * The term frequency is, in fact, just the same as the frequency of the WordNet sense with the
   * same offset, but different term. For example, "anticipate" and "expect" both have the same
   * WordNet sense with offset 00721658, but there are two different sense entries for that offset:
   * expect%2:31:00:: and anticipate%2:31:00::. Those sense entries have different frequencies - 204
   * and 8, respectively. Technically this information is redundant, because it is elsewhere in the
   * table, but it may prove convenient anyway. 
   * 
   * @param senseIds
   * @return
   */
  private List<SenseTableEntry> makeTroponymTable() {
    List<SenseTableEntry> troponymTable = new ArrayList<SenseTableEntry>();
    for (String propBankSense : senseIds.keySet()) {
      for (ISenseKey senseKey : senseIds.get(propBankSense)) {
        ISenseEntry senseEntry = wordNet.getSenseEntry(senseKey);
        ISynsetID synsetId = new SynsetID(senseEntry.getOffset(), senseEntry.getPOS());
        ISynset synset = wordNet.getSynset(synsetId);
        for (ISynsetID relatedSynsetId : synset.getRelatedSynsets(Pointer.HYPONYM)) {
          ISynset relatedSynset = wordNet.getSynset(relatedSynsetId);
          for (IWord word : relatedSynset.getWords()) {
            ISenseKey termSenseKey = new SenseKey(word.getLemma(), word.getLexicalID(), relatedSynset);
            ISenseEntry termSenseEntry = wordNet.getSenseEntry(termSenseKey);
            int termFrequency = 0;
            if (termSenseEntry != null) {
              termFrequency = termSenseEntry.getTagCount();
            }
            troponymTable.add(new SenseTableEntry(
              propBankSense, senseKey, senseEntry.getTagCount(), word.getLemma(), termFrequency,
              termSenseKey
            ));
          }
        }
      }
    }
    return troponymTable;
  }
  
  private void makeWordToSenseMap() {
    Map<String, Map<ISenseKey, Integer>> wordSenseMap = new HashMap<String, Map<ISenseKey, Integer>>();
    
    Iterator<IIndexWord> indexWordIterator = wordNet.getIndexWordIterator(POS.VERB);
    while (indexWordIterator.hasNext()) {
      IIndexWord indexWord = indexWordIterator.next();
      for (IWordID wordId : indexWord.getWordIDs()) {
        IWord word = wordNet.getWord(wordId);
        ISenseKey senseKey = word.getSenseKey();
        ISenseEntry senseEntry = wordNet.getSenseEntry(senseKey);
        int count = senseEntry.getTagCount();
        
        ISynsetID synsetId = new SynsetID(senseEntry.getOffset(), POS.VERB);
        ISynset synset = wordNet.getSynset(synsetId);
        
        List<IWord> allWords = new ArrayList<IWord>(synset.getWords());
        allWords.add(word);
        for (IWord synWord : synset.getWords()) {
          if (wordSenseMap.containsKey(synWord.getLemma())) {
            Map<ISenseKey, Integer> senseCounts = wordSenseMap.get(synWord.getLemma());
            if (senseCounts.containsKey(senseKey)) {
              senseCounts.put(senseKey, senseCounts.get(senseKey) + count);
            } else {
              senseCounts.put(senseKey, count);
            }
            wordSenseMap.put(synWord.getLemma(), senseCounts);
          } else {
            Map<ISenseKey, Integer> senseCounts = new HashMap<ISenseKey, Integer>();
            senseCounts.put(senseKey, count);
            wordSenseMap.put(synWord.getLemma(), senseCounts);
          }
        }
      }
    }
    
    // Output
    try {
      BufferedWriter writer = new BufferedWriter(new FileWriter(
        outputDir + "words-to-senses.tsv"
      ));
      
      for (String word : wordSenseMap.keySet()) {
        Map<ISenseKey, Integer> senseCounts = wordSenseMap.get(word);
        for (ISenseKey senseKey : senseCounts.keySet()) {
          int count = senseCounts.get(senseKey);
          writer.write(word + "\t" + senseKey + "\t" + count + "\n");
        }
      }
      writer.close();
    } catch (IOException e) {
      throw new RuntimeException(e);
    }
  }
  
  /**
   * Flatten the PropBank-to-words / Words-to-PropBank map into a tab-separated file with columns:
   * PropBank sense, string, count (or String, PropBank sense, count).
   */
  private void outputMap(Map<String, Map<String, Integer>> map, BufferedWriter writer) {
    for (String key : map.keySet()) {
      Map<String, Integer> valueCounts = map.get(key);
      int valueSum = 0;
      for (int count : valueCounts.values()) {
        valueSum += count;
      }
      for (String value : valueCounts.keySet()) {
        int count = valueCounts.get(value);
        double cprob = 0;
        if (valueSum != 0) {
          cprob = (double) count / valueSum;
        }
        double smoothedCprob = (double) (count + 1) / (valueSum + valueCounts.size());
        String line = new StringBuilder(key).append("\t")
          .append(value).append("\t")
          .append(count).append("\t")
          .append(cprob).append("\t")
          .append(smoothedCprob).append("\n")
          .toString();
        try {
          writer.write(line);
        } catch (IOException e) {
          throw new RuntimeException("Error writing output map. ", e);
        }
      }
    }
  }
  
  /**
   * Print out the PropBank-to-sense table.
   */
  private void outputSenseTable(List<SenseTableEntry> senseTable, BufferedWriter writer) {
    for (SenseTableEntry entry : senseTable) {
      String line = new StringBuilder(entry.getPropBankSense()).append("\t")
        .append(entry.getSenseKey()).append("\t")
        .append(entry.getSenseFrequency()).append("\t")
        .append(entry.getTerm()).append("\t")
        .append(entry.getTermFrequency()).append("\t")
        .append(entry.getTermSenseKey()).append("\n")
        .toString();
      try {
        writer.write(line);
      } catch (IOException e) {
        throw new RuntimeException("Error writing sense table. ", e);
      }
    }
  }
  
  /**
   * Makes a table from PropBank senses to PropBank senses. For each PropBank sense, get the list of
   * synsets associated with it. Then find the PropBank senses associated with those synsets.
   */
  private void outputPropBankTable(BufferedWriter writer) {
    for (String propBankSense : senseIds.keySet()) {
      for (ISenseKey senseKey : senseIds.get(propBankSense)) {
        ISenseEntry senseEntry = wordNet.getSenseEntry(senseKey);
        ISynsetID synsetId = new SynsetID(senseEntry.getOffset(), senseEntry.getPOS());
        int tagCount = senseEntry.getTagCount();
        
        for (String synonymousPropBankSense : senseIdsInverse.get(synsetId)) {
          String line = new StringBuilder(propBankSense).append("\t")
            .append(synonymousPropBankSense).append("\t")
            .append(tagCount).append("\n")
            .toString();
          try {
            writer.write(line);
          } catch (IOException e) {
            throw new RuntimeException("Error writing propbank to propbank map. ", e);
          }
        }
      }
    }
  }
  
  /**
   * Makes a table from PropBank senses to PropBank senses. For each PropBank sense, get the list of
   * troponyms and their synsets. Then find the PropBank senses associated with those synsets.
   */
  private void outputPropBankTroponymTable(BufferedWriter writer) {
    for (String propBankSense : senseIds.keySet()) {
      for (ISenseKey senseKey : senseIds.get(propBankSense)) {
        ISenseEntry senseEntry = wordNet.getSenseEntry(senseKey);
        ISynsetID synsetId = new SynsetID(senseEntry.getOffset(), senseEntry.getPOS());
        ISynset synset = wordNet.getSynset(synsetId);
        int tagCount = senseEntry.getTagCount();
        
        for (ISynsetID relatedSynsetId : synset.getRelatedSynsets(Pointer.HYPONYM)) {
          if (!senseIdsInverse.containsKey(relatedSynsetId)) {
            continue;
          }
          for (String troponymPropBankSense : senseIdsInverse.get(relatedSynsetId)) {
            String line = new StringBuilder(propBankSense).append("\t")
              .append(senseEntry.getOffset()).append("\t")
              .append(relatedSynsetId.getOffset()).append("\t")
              .append(troponymPropBankSense).append("\n")
              .toString();
            try {
              writer.write(line);
            } catch (IOException e) {
              throw new RuntimeException("Error writing propbank to propbank map. ", e);
            }
          }
        }
      }
    }
  }
  
  /**
   * Makes table from PropBank senses to synonymous PropBank senses.
   */
  private Map<String, Set<String>> getPropbankToPropbankSynonyms() {
    Map<String, Set<String>> propbankTroponymsToSenses = new HashMap<String, Set<String>>();
    String propbankSynonymPath = outputDir + "propbank-to-propbank-synonyms.tsv";
    try {
      BufferedReader reader = new BufferedReader(new FileReader(propbankSynonymPath));
      while(reader.ready()) {
        String line = reader.readLine();
        String[] columns = line.split("\t");
        String propbankSense = columns[0].trim();
        String propbankSynonym = columns[2].trim();
        if (propbankTroponymsToSenses.containsKey(propbankSynonym)) {
          propbankTroponymsToSenses.get(propbankSynonym).add(propbankSense);
        } else {
          Set<String> propbankSenses = new HashSet<String>();
          propbankSenses.add(propbankSense);
          propbankTroponymsToSenses.put(propbankSynonym, propbankSenses);
        }
      }
      reader.close();
    } catch (Exception e) {
      throw new RuntimeException(e);
    }
    return propbankTroponymsToSenses;
  }
  
  /**
   * Makes table from PropBank senses to synonymous verb phrases.
   */
  private Map<String, Set<String>> getStringToPropbankSynonyms() {
    Map<String, Set<String>> stringToPropbankSenses = new HashMap<String, Set<String>>();
    String propbankTroponymPath = outputDir + "propbank-to-synonyms.tsv";
    try {
      BufferedReader reader = new BufferedReader(new FileReader(propbankTroponymPath));
      while(reader.ready()) {
        String line = reader.readLine();
        String[] columns = line.split("\t");
        String propbankSense = columns[0].trim();
        int count = Integer.parseInt(columns[2].trim());
        String string = columns[3].trim();
        if (stringToPropbankSenses.containsKey(string)) {
          stringToPropbankSenses.get(string).add(propbankSense);
        } else {
          Set<String> propbankSenses = new HashSet<String>();
          propbankSenses.add(propbankSense);
          stringToPropbankSenses.put(string, propbankSenses);
        }
      }
      reader.close();
    } catch (Exception e) {
      throw new RuntimeException(e);
    }
    return stringToPropbankSenses;
  }
  
  private Map<String, Set<String>> getPropbankToPropbankEntailments() {
    Map<String, Set<String>> propbankTroponymsToSenses = new HashMap<String, Set<String>>();
    String propbankTroponymPath = outputDir + "propbank-to-propbank-troponyms.tsv";
    try {
      BufferedReader reader = new BufferedReader(new FileReader(propbankTroponymPath));
      while(reader.ready()) {
        String line = reader.readLine();
        String[] columns = line.split("\t");
        String propbankSense = columns[0].trim();
        String propbankTroponym = columns[3].trim();
        if (propbankTroponymsToSenses.containsKey(propbankTroponym)) {
          propbankTroponymsToSenses.get(propbankTroponym).add(propbankSense);
        } else {
          Set<String> propbankSenses = new HashSet<String>();
          propbankSenses.add(propbankSense);
          propbankTroponymsToSenses.put(propbankTroponym, propbankSenses);
        }
      }
      reader.close();
    } catch (Exception e) {
      throw new RuntimeException(e);
    }
    return propbankTroponymsToSenses;
  }
  
  /**
   * Makes table from PropBank senses to PropBank troponyms.
   */
  private Map<String, Set<String>> getStringToPropbankEntailments() {
    Map<String, Set<String>> stringToPropbankSenses = new HashMap<String, Set<String>>();
    String propbankTroponymPath = outputDir + "propbank-to-troponyms.tsv";
    try {
      BufferedReader reader = new BufferedReader(new FileReader(propbankTroponymPath));
      while(reader.ready()) {
        String line = reader.readLine();
        String[] columns = line.split("\t");
        String string = columns[3].trim();
        String propbankSense = columns[0].trim();
        if (stringToPropbankSenses.containsKey(string)) {
          stringToPropbankSenses.get(string).add(propbankSense);
        } else {
          Set<String> propbankSenses = new HashSet<String>();
          propbankSenses.add(propbankSense);
          stringToPropbankSenses.put(string, propbankSenses);
        }
      }
      reader.close();
    } catch (Exception e) {
      throw new RuntimeException(e);
    }
    return stringToPropbankSenses;
  }
  
  /**
   * Represents a path in the Propbank transitive closure.
   */
  class TransitiveClosurePath {
    LinkedList<String> path;
    Set<String> elements;
    
    public TransitiveClosurePath() {
      path = new LinkedList<String>();
      elements = new HashSet<String>();
    }
    
    public TransitiveClosurePath(TransitiveClosurePath other) {
      path = new LinkedList<String>(other.path);
      elements = new HashSet<String>(other.elements);
    }
    
    public void add(String element) {
      path.add(element);
      elements.add(element);
    }
    
    public String getLast() {
      return path.getLast();
    }
    
    public int getLength() {
      return path.size();
    }
    
    public boolean containsElement(String element) {
      return elements.contains(element);
    }
    
    public LinkedList<String> getPath() {
      return path;
    }

    @Override
    public int hashCode() {
      final int prime = 31;
      int result = 1;
      result = prime * result + getOuterType().hashCode();
      result = prime * result + ((elements == null) ? 0 : elements.hashCode());
      result = prime * result + ((path == null) ? 0 : path.hashCode());
      return result;
    }

    @Override
    public boolean equals(Object obj) {
      if (this == obj) return true;
      if (obj == null) return false;
      if (getClass() != obj.getClass()) return false;
      TransitiveClosurePath other = (TransitiveClosurePath) obj;
      if (!getOuterType().equals(other.getOuterType())) return false;
      if (elements == null) {
        if (other.elements != null) return false;
      } else if (!elements.equals(other.elements)) return false;
      if (path == null) {
        if (other.path != null) return false;
      } else if (!path.equals(other.path)) return false;
      return true;
    }

    private WordSenseMapper getOuterType() {
      return WordSenseMapper.this;
    }
  }
  
  private void findPropBankTransitiveClosure() {
    boolean useEntailments = true;
    boolean useSynonyms = true;
    int maxPathLength = 3;
    Map<String, Set<String>> stringToPropbankEntailments = getStringToPropbankEntailments();
    Map<String, Set<String>> propbankToPropbankEntailments = getPropbankToPropbankEntailments();
    Map<String, Set<String>> stringToPropbankSynonyms = getStringToPropbankSynonyms();
    Map<String, Set<String>> propbankToPropbankSynonyms = getPropbankToPropbankSynonyms();
    
    Set<String> testStrings = new HashSet<String>();
    testStrings.addAll(stringToPropbankEntailments.keySet());
    testStrings.addAll(stringToPropbankSynonyms.keySet());
    
    for (String testString : testStrings) {
      LinkedList<TransitiveClosurePath> queue = new LinkedList<TransitiveClosurePath>();
      if (useEntailments && stringToPropbankEntailments.containsKey(testString)) {
        for (String propbankSense : stringToPropbankEntailments.get(testString)) {
          TransitiveClosurePath path = new TransitiveClosurePath();
          path.add(propbankSense);
          queue.add(path);
        }
      }
      if (useSynonyms && stringToPropbankSynonyms.containsKey(testString)) {
       for (String propbankSense : stringToPropbankSynonyms.get(testString)) {
          TransitiveClosurePath path = new TransitiveClosurePath();
          path.add(propbankSense);
          queue.add(path);
        }
      }
      
      Set<TransitiveClosurePath> transitiveClosure = new HashSet<TransitiveClosurePath>();
      Set<String> lastPbSenses = new TreeSet<String>();
      while (!queue.isEmpty()) {
        TransitiveClosurePath path = queue.removeFirst();
        transitiveClosure.add(path);
        lastPbSenses.add(path.getLast());
        
        if (path.getLength() >= maxPathLength) {
          continue;
        }
        
        String lastPbSense = path.getLast();
        Set<String> nextPbSenses = new HashSet<String>();
        if (useEntailments && propbankToPropbankEntailments.containsKey(lastPbSense)) {
          nextPbSenses.addAll(propbankToPropbankEntailments.get(lastPbSense));
        }
        if (useSynonyms && propbankToPropbankSynonyms.containsKey(lastPbSense)) {
          nextPbSenses.addAll(propbankToPropbankSynonyms.get(lastPbSense));
        }
        for (String nextPbSense : nextPbSenses) {
          if (!path.containsElement(nextPbSense)) {
            TransitiveClosurePath newPath = new TransitiveClosurePath(path);
            newPath.add(nextPbSense);
            queue.add(newPath);
          }
        }
      }
      
      try {
        File outputFile = new File(outputDir + "tc/" + testString + ".tsv");
        outputFile.createNewFile();
        BufferedWriter writer = new BufferedWriter(new FileWriter(outputFile));
        writer.write("Final senses:\n");
        for (String lastPbSense : lastPbSenses) {
          writer.write(lastPbSense + "\n");
        }
        writer.write("\n");
        writer.write("All paths:\n");
        String[] tcArray = new String[transitiveClosure.size()];
        int index = 0;
        for (TransitiveClosurePath path : transitiveClosure) {
          tcArray[index++] = path.getPath().toString();
        }
        Arrays.sort(tcArray);
        for(String path : tcArray) {
          writer.write(path + "\n");
        }
        writer.close();
      } catch (IOException e) {
        throw new RuntimeException(e);
      }
    }
  }
  
  /**
   * Quick and dirty method that creates a benchmark set of Open IE tuples.
   */
  private void createBenchmark() {
    String openiePath = inputDir + "openie_100.txt";
    Map<String, Set<String>> stringToPropbankEntailments = getStringToPropbankEntailments();
    Map<String, Set<String>> stringToPropbankSynonyms = getStringToPropbankSynonyms();
    
    try {
      BufferedReader reader = new BufferedReader(new FileReader(openiePath));
      while(reader.ready()) {
        String line = reader.readLine();
        String[] columns = line.split("\t");
        
        String arg1 = columns[0];
        String relPhrase = columns[1];
        String arg2 = columns[2];
        String srl = columns[7];
        String sentence = columns[11];

        String[] relWords = relPhrase.split(" ");
        int numWordsInRel = relWords.length;
        int relStart = Integer.parseInt(columns[9].substring(1, columns[9].length()-1).split(",")[0]);
        int relEnd = relStart + numWordsInRel; // exclusive
        String[] posTags = columns[12].split(" ");
        List<String> verbs = new ArrayList<String>();
        for (int i=relStart, count=0; i<relEnd; i++) {
          String posTag = posTags[i];
          if (posTag.startsWith("V")) {
            verbs.add(relWords[count]);
            count++;
          } else {
            if (verbs.size() == 0) {
              count++;
              continue;
            } else {
              break;
            }
          }
        }
        String verb = join(verbs, " ");
        if (verb == null) {
          continue;
        }
        
        Set<String> propbankSyns = stringToPropbankSynonyms.get(verb);
        Set<String> propbankTrops = stringToPropbankEntailments.get(verb);
        Set<String> ontoSyns = new HashSet<String>();
        Set<String> ontoTrops = new HashSet<String>();
        String directSynonyms = null;
        String directTrops = null;
        if (propbankSyns != null) {
          directSynonyms = join(propbankSyns, ", ");
          ontoSyns = ontologyLookup(propbankSyns);
          ontoSyns.removeAll(propbankSyns);
        }
        if (propbankTrops != null) {
          directTrops = join(propbankTrops, ", ");
          ontoTrops = ontologyLookup(propbankTrops);
          ontoTrops.removeAll(propbankTrops);
        }
        Set<String> ontoSenses = new TreeSet<String>();
        ontoSenses.addAll(ontoSyns);
        ontoSenses.addAll(ontoTrops);
        String ontoSensesStr = join(ontoSenses, ", ");
        
        String directSenses = join(Arrays.asList(directSynonyms, directTrops), "; ");
        System.out.println(join(Arrays.asList(
          "Arg 1: " + arg1,
          "Relation: " + relPhrase,
          "Arg 2: " + arg2,
          "Verb: " + verb,
          "SRL link: " + srl,
          "Sentence: " + sentence,
          "Gold senses: ",
          "Direct senses: " + directSenses,
          "Ontology senses: " + ontoSensesStr), "\n"));
        System.out.println();
      }
      reader.close();
    } catch (IOException e) {
      throw new RuntimeException(e);
    }
  }
  
  /**
   * Gets the set of entailed and synonymous PropBank senses for each PropBank sense in the given
   * set.
   */
  private Set<String> ontologyLookup(Set<String> pbSenses) {
    Set<String> set = new HashSet<String>();
    Map<String, Set<String>> propbankToPropbankEntailments = getPropbankToPropbankEntailments();
    Map<String, Set<String>> propbankToPropbankSynonyms = getPropbankToPropbankSynonyms();
    for (String pbSense : pbSenses) {
      Set<String> entailments = propbankToPropbankEntailments.get(pbSense);
      Set<String> synonyms = propbankToPropbankSynonyms.get(pbSense);
      if (entailments != null) {
        set.addAll(entailments);
      }
      if (synonyms != null) {
        set.addAll(synonyms);
      }
    }
    return set;
  }
  
  /**
   * Generic string join function.
   */
  public String join(Iterable<String> list, String str) {
    StringBuilder builder = new StringBuilder();
    boolean first = true;
    for (String item : list) {
      if (item == null) {
        continue;
      }
      if (!first) {
        builder.append(str);
      }
      builder.append(item);
      first = false;
    }
    return builder.toString();
  }
  
  /**
   * Executes the given experiments.
   */
  public void run(Experiment[] experiments) {
    readPropBankToWordNetMapping();
    
    String wordsPath = outputDir + "propbank-to-words.tsv";
    String inversePath = outputDir + "propbank-to-words-inverse.tsv";
    String tablePath = outputDir + "propbank-to-synonyms.tsv";
    String propBankSynonymPath = outputDir + "propbank-to-propbank-synonyms.tsv";
    String troponymPath = outputDir + "propbank-to-troponyms.tsv";
    String propBankTroponymPath = outputDir + "propbank-to-propbank-troponyms.tsv";
    
    for (Experiment experiment : experiments) {
      switch (experiment) {
        case MAKE_TABLES:
          Map<String, Map<String, Integer>> synonymMap = makeSynonymMap();
          Map<String, Map<String, Integer>> inverseMap = makeInverseMap(synonymMap);
          List<SenseTableEntry> senseTable = makeSenseTable();
          List<SenseTableEntry> troponymTable = makeTroponymTable();
          makeWordToSenseMap();
          try {
            BufferedWriter outputWriter = new BufferedWriter(new FileWriter(wordsPath));
            BufferedWriter inverseWriter = new BufferedWriter(new FileWriter(inversePath));
            BufferedWriter tableWriter = new BufferedWriter(new FileWriter(tablePath));
            BufferedWriter propBankSynWriter = new BufferedWriter(new FileWriter(propBankSynonymPath));
            BufferedWriter troponymWriter = new BufferedWriter(new FileWriter(troponymPath));
            BufferedWriter propBankTropWriter = new BufferedWriter(new FileWriter(propBankTroponymPath));
            outputMap(synonymMap, outputWriter);
            outputMap(inverseMap, inverseWriter);
            outputSenseTable(senseTable, tableWriter);
            outputPropBankTable(propBankSynWriter);
            outputSenseTable(troponymTable, troponymWriter);
            outputPropBankTroponymTable(propBankTropWriter);
            outputWriter.close();
            inverseWriter.close();
            tableWriter.close();
            propBankSynWriter.close();
            troponymWriter.close();
            propBankTropWriter.close();
          } catch (IOException e) {
            throw new RuntimeException("Error writing output files.", e);
          }
          break;
        case COMPUTE_TRANSITIVE_CLOSURE:
          findPropBankTransitiveClosure();
          break;
        case CREATE_BENCHMARK:
          createBenchmark();
          break;
      }
    }
  }
  
  enum Experiment {
    MAKE_TABLES, COMPUTE_TRANSITIVE_CLOSURE, CREATE_BENCHMARK;
  }
  
  /**
   * Parses args and starts the program.
   * @param args
   *    inputDir: The directory with all the input files in it.
   *    outputDir: The directory where all the output files should be.
   */
  public static void main(String[] args) {
    if (args.length != 2) {
      System.out.println(
        "Usage: java edu.washington.cs.knowitall.relation.preprocess.WordSenseMapper inputDir"
        + " outputDir"
      );
      return;
    }
    String inputDir = args[0];
    String outputDir = args[1];

    WordSenseMapper mapper = new WordSenseMapper(inputDir, outputDir);
    Experiment[] experiments = {Experiment.CREATE_BENCHMARK};
    mapper.run(experiments);
  }
}
