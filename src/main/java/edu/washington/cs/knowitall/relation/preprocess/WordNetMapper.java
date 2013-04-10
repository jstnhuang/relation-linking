package edu.washington.cs.knowitall.relation.preprocess;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.io.Writer;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import edu.mit.jwi.Dictionary;
import edu.mit.jwi.IDictionary;
import edu.mit.jwi.item.IIndexWord;
import edu.mit.jwi.item.ISynset;
import edu.mit.jwi.item.ISynsetID;
import edu.mit.jwi.item.IWord;
import edu.mit.jwi.item.IWordID;
import edu.mit.jwi.item.POS;
import edu.mit.jwi.item.Pointer;

/**
 * Given a mapping from PropBank senses to WordNet sense numbers, expand the WordNet senses into
 * synonyms and troponyms.
 */
public class WordNetMapper {
  IDictionary wordNet;
  String inputDir;
  String outputDir;
  
  public WordNetMapper(String inputDir, String outputDir) {
    this.inputDir = inputDir;
    this.outputDir = outputDir;
    
    String wordNetPath = inputDir + "WordNet-3.0/dict/";
    try {
      wordNet = new Dictionary(new File(wordNetPath));
      wordNet.open();
    } catch (Exception e) {
      throw new RuntimeException(e);
    }
  }
  
  /**
   * Reads a mapping from PropBank senses to WordNet sense numbers, and bundles together the
   * synonyms and troponyms (aka hyponyms) for that word with the PropBank sense.
   */
  private List<PbToWnMapping> getPbToWnMappings() {
    String mappingPath = inputDir + "pb-wn.tsv";
    List<PbToWnMapping> pbToWnMappings = new ArrayList<>();
    try {
      BufferedReader reader = new BufferedReader(new FileReader(mappingPath));
      while (reader.ready()) {
        String line = reader.readLine();
        String[] columns = line.split("\t");
        String pbSense = columns[0];
        String lemma = columns[1];
        int wnSenseNumber = Integer.parseInt(columns[2]);
        
        IIndexWord idxWord = wordNet.getIndexWord(lemma, POS.VERB);
        IWordID wordId = idxWord.getWordIDs().get(wnSenseNumber-1);
        IWord word = wordNet.getWord(wordId);
        
        ISynset synset = word.getSynset();
        Map<IWord, Integer> synonymCounts = new HashMap<>();
        for (IWord synonym : synset.getWords()) {
          int count = wordNet.getSenseEntry(synonym.getSenseKey()).getTagCount();
          synonymCounts.put(synonym, count);
        }
        
        List<ISynsetID> hyponymSynsetIds = synset.getRelatedSynsets(Pointer.HYPONYM);
        Map<IWord, Integer> hyponymCounts = new HashMap<>();
        for (ISynsetID hyponymSynsetId : hyponymSynsetIds) {
          ISynset hyponymSynset = wordNet.getSynset(hyponymSynsetId);
          for (IWord hyponym : hyponymSynset.getWords()) {
            int count = wordNet.getSenseEntry(hyponym.getSenseKey()).getTagCount();
            hyponymCounts.put(hyponym, count);
          }
        }
        
        pbToWnMappings.add(new PbToWnMapping(pbSense, word, synonymCounts, hyponymCounts));
      }
      reader.close();
    } catch (IOException e) {
      throw new RuntimeException(e);
    }
    
    return pbToWnMappings;
  }
  
  /**
   * Flattens the related counts and outputs the rows with the given writer.
   */
  private void writeRelatedCounts(Writer writer, String pbSense, IWord word,
      Map<IWord, Integer> relatedCounts) throws IOException {
    for (IWord relatedWord : relatedCounts.keySet()) {
      int count = relatedCounts.get(relatedWord);
      String line = new StringBuilder(pbSense).append("\t")
        .append(word.getSenseKey()).append("\t")
        .append(relatedWord.getLemma()).append("\t")
        .append(relatedWord.getSenseKey()).append("\t")
        .append(count).append("\n").toString();
      writer.write(line);
    }
  }
  
  /**
   * Outputs the given PropBank to WordNet mapping.
   */
  private void outputPbToWnMappings(List<PbToWnMapping> pbToWnMappings) {
    String synonymOutputPath = outputDir + "propbank-to-synonyms.tsv";
    String hyponymOutputPath = outputDir + "propbank-to-hyponyms.tsv";
    try {
      Writer synonymWriter = new BufferedWriter(new FileWriter(synonymOutputPath));
      Writer hyponymWriter = new BufferedWriter(new FileWriter(hyponymOutputPath));
      
      for (PbToWnMapping pbToWnMapping : pbToWnMappings) {
        String pbSense = pbToWnMapping.getPbSense();
        IWord word = pbToWnMapping.getWord();
        Map<IWord, Integer> synonymCounts = pbToWnMapping.getSynonymCounts();
        Map<IWord, Integer> hyponymCounts = pbToWnMapping.getHyponymCounts();
        writeRelatedCounts(synonymWriter, pbSense, word, synonymCounts);
        writeRelatedCounts(hyponymWriter, pbSense, word, hyponymCounts);
      }
      
      synonymWriter.close();
      hyponymWriter.close();
    } catch (IOException e) {
      throw new RuntimeException(e);
    }
  }
  
  public void run() {
    List<PbToWnMapping> pbToWnMappings = getPbToWnMappings();
    outputPbToWnMappings(pbToWnMappings);
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

    WordNetMapper mapper = new WordNetMapper(inputDir, outputDir);
    mapper.run();
  }
}

class PbToWnMapping {
  String pbSense;
  IWord wordId;
  Map<IWord, Integer> synonymCounts;
  Map<IWord, Integer> hyponymCounts;
  public PbToWnMapping(String pbSense, IWord wordId, Map<IWord, Integer> synonymCounts,
      Map<IWord, Integer> hyponymCounts) {
    super();
    this.pbSense = pbSense;
    this.wordId = wordId;
    this.synonymCounts = synonymCounts;
    this.hyponymCounts = hyponymCounts;
  }
  public String getPbSense() {
    return pbSense;
  }
  public IWord getWord() {
    return wordId;
  }
  public Map<IWord, Integer> getSynonymCounts() {
    return synonymCounts;
  }
  public Map<IWord, Integer> getHyponymCounts() {
    return hyponymCounts;
  }
}
