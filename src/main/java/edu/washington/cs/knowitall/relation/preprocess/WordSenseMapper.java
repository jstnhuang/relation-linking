package edu.washington.cs.knowitall.relation.preprocess;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.util.HashMap;
import java.util.HashSet;
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
  BufferedWriter outputWriter;
  BufferedWriter inverseWriter;
  
  public WordSenseMapper(BufferedReader mapReader, IDictionary wordNet,
      BufferedWriter outputWriter, BufferedWriter inverseWriter) {
    this.mapReader = mapReader;
    this.wordNet = wordNet;
    this.outputWriter = outputWriter;
    this.inverseWriter = inverseWriter;
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
   * and add each to the synonym map, along with its count.
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
   * Flatten the synonym/inverse map into a tab-separated file with columns: PropBank sense, string,
   * count.
   */
  private void outputMap(Map<String, Map<String, Integer>> map, BufferedWriter writer) {
    for (String key : map.keySet()) {
      Map<String, Integer> valueCounts = map.get(key);
      for (String value : valueCounts.keySet()) {
        int count = valueCounts.get(value);
        String line = new StringBuilder(key).append("\t").append(value).append("\t")
          .append(count).append("\n").toString();
        try {
          writer.write(line);
        } catch (IOException e) {
          throw new RuntimeException("Error writing output map. ", e);
        }
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
    outputMap(synonymMap, outputWriter);
    outputMap(inverseMap, inverseWriter);
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
    String inverseFilePath = args[2] + "-inverse";

    try {
      BufferedReader mapReader = new BufferedReader(new FileReader(pbToWnPath));
      IDictionary wordNet = new Dictionary(new File(wordNetPath));
      wordNet.open();
      BufferedWriter outputWriter = new BufferedWriter(new FileWriter(outputFilePath));
      BufferedWriter inverseWriter = new BufferedWriter(new FileWriter(inverseFilePath));
      WordSenseMapper mapper = new WordSenseMapper(mapReader, wordNet, outputWriter, inverseWriter);
      mapper.run();
      mapReader.close();
      outputWriter.close();
      inverseWriter.close();
    } catch (Exception e) {
      throw new RuntimeException(e);
    }
  }
}
