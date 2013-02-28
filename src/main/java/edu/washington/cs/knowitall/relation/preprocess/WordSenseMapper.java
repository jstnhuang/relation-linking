package edu.washington.cs.knowitall.relation.preprocess;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import edu.mit.jwi.Dictionary;
import edu.mit.jwi.IDictionary;
import edu.mit.jwi.data.parse.SenseKeyParser;
import edu.mit.jwi.item.ISenseEntry;
import edu.mit.jwi.item.ISenseKey;
import edu.mit.jwi.item.ISynset;
import edu.mit.jwi.item.ISynsetID;
import edu.mit.jwi.item.IWord;
import edu.mit.jwi.item.SenseKey;
import edu.mit.jwi.item.SynsetID;

/**
 * Builds a mapping from PropBank rolesets (which we refer to sometimes as PropBank senses) to
 * WordNet synonym sets and vice versa. To do this, we need two data sources:
 * <ul>
 *  <li>A mapping from PropBank senses to WordNet sense IDs (provided by VerbNet)</li>
 *  <li>WordNet itself</li>
 * </ul>
 * Outputs a flat, tab-separated file suitable for human examination or database import.
 */
public class WordSenseMapper {
  BufferedReader mapReader;
  IDictionary wordNet;
  String outputPath;
  
  public WordSenseMapper(BufferedReader mapReader, IDictionary wordNet, String outputPath) {
    this.mapReader = mapReader;
    this.wordNet = wordNet;
    this.outputPath = outputPath;
  }
  
  /**
   * Read the PropBank senses -> WordNet senses mapping from file.
   * 
   * Assumes the map is a tab-separated file with columns: verb, PropBank senses, VerbNet group,
   * WordNet senses. The PropBank senses and WordNet senses are separated by spaces. Also, assumes
   * the WordNet senses are missing the last two colon-separated fields regarding the head word, so
   * we just add "::" to the end of each.
   * 
   * @return A map from PropBank senses to the set of all WordNet senses associated with it.
   */
  private Map<String, Set<ISenseKey>> readMapping(BufferedReader mapReader) {
    Map<String, Set<ISenseKey>> senseIds = new HashMap<String, Set<ISenseKey>>();
    try {
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
          }
        }
      }
    } catch (IOException e) {
      throw new RuntimeException("Error reading mapping file.", e);
    }
    return senseIds;
  }
  
  /**
   * Given the PropBank sense to WordNet sense mapping, expand the synonyms from the WordNet sense
   * and add each to the synonym map, along with its count. This gives us a map from PropBank senses
   * to terms, as well as the count of a term given the PropBank sense.
   */
  private Map<String, Map<String, Integer>> makeSynonymMap(Map<String, Set<ISenseKey>> senseIds) {
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
        System.out.println(senseEntry.getOffset() == synset.getOffset());
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
  private List<SenseTableEntry> makeSenseTable(Map<String, Set<ISenseKey>> senseIds) {
    List<SenseTableEntry> senseTable = new ArrayList<SenseTableEntry>();
    for (String propBankSense : senseIds.keySet()) {
      for (ISenseKey senseKey : senseIds.get(propBankSense)) {
        ISenseEntry senseEntry = wordNet.getSenseEntry(senseKey);
        ISynsetID id = new SynsetID(senseEntry.getOffset(), senseEntry.getPOS());
        ISynset synset = wordNet.getSynset(id);
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
   * Flatten the synonym/inverse map into a tab-separated file with columns: PropBank sense, string,
   * count.
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
   * Print out the sense table
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
   * Execute the program.
   */
  public void run() {
    Map<String, Set<ISenseKey>> senseIds = readMapping(mapReader);
    Map<String, Map<String, Integer>> synonymMap = makeSynonymMap(senseIds);
    Map<String, Map<String, Integer>> inverseMap = makeInverseMap(synonymMap);
    List<SenseTableEntry> senseTable = makeSenseTable(senseIds);
    String inversePath = outputPath + "-inverse";
    String tablePath = outputPath + "-table";
    BufferedWriter outputWriter;
    try {
      outputWriter = new BufferedWriter(new FileWriter(outputPath));
      BufferedWriter inverseWriter = new BufferedWriter(new FileWriter(inversePath));
      BufferedWriter tableWriter = new BufferedWriter(new FileWriter(tablePath));
      outputMap(synonymMap, outputWriter);
      outputMap(inverseMap, inverseWriter);
      outputSenseTable(senseTable, tableWriter);
      outputWriter.close();
      inverseWriter.close();
      tableWriter.close();
    } catch (IOException e) {
      throw new RuntimeException("Error writing output files.", e);
    }
  }
  
  /**
   * Parses args and starts the program.
   * @param args
   *    pbToWnFile: The file mapping PropBank senses to WordNet senses. See {@link getMapping}.
   *    wordNetPath: The path of the WordNet dict folder.
   *    outputFile: The name of the output file. The inverse will be given the same name, but with
   *        -inverse added to it.
   */
  public static void main(String[] args) {
    if (args.length != 3) {
      System.out.println(
        "Usage: java edu.washington.cs.knowitall.relation.preprocess.WordSenseMapper pbToWnFile"
        + " wordNetPath outputFile"
      );
    }
    String pbToWnPath = args[0];
    String wordNetPath = args[1];
    String outputFilePath = args[2];

    try {
      BufferedReader mapReader = new BufferedReader(new FileReader(pbToWnPath));
      IDictionary wordNet = new Dictionary(new File(wordNetPath));
      wordNet.open();
      WordSenseMapper mapper = new WordSenseMapper(mapReader, wordNet, outputFilePath);
      mapper.run();
      mapReader.close();
    } catch (Exception e) {
      throw new RuntimeException(e);
    }
  }
}
