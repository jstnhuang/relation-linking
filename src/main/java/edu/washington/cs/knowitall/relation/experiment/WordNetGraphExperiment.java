package edu.washington.cs.knowitall.relation.experiment;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.util.HashMap;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.Map;
import java.util.Set;
import java.util.TreeMap;
import java.util.TreeSet;

import edu.mit.jwi.IDictionary;
import edu.mit.jwi.IRAMDictionary;
import edu.mit.jwi.RAMDictionary;
import edu.mit.jwi.data.ILoadPolicy;
import edu.mit.jwi.item.IIndexWord;
import edu.mit.jwi.item.ISenseEntry;
import edu.mit.jwi.item.ISenseKey;
import edu.mit.jwi.item.ISynset;
import edu.mit.jwi.item.ISynsetID;
import edu.mit.jwi.item.IWord;
import edu.mit.jwi.item.IWordID;
import edu.mit.jwi.item.POS;
import edu.mit.jwi.item.SynsetID;

public class WordNetGraphExperiment {
  IDictionary wordNet;
  String outputPath;
  
  public WordNetGraphExperiment(IDictionary wordNet, String outputPath) {
    this.wordNet = wordNet;
    this.outputPath = outputPath;
  }
  
  /**
   * Finds connected components of word senses, through the synsets.
   */
  private void findConnectedComponents(POS partOfSpeech) {
    Map<ISenseKey, Integer> senseComponents = new HashMap<ISenseKey, Integer>();
    
    Iterator<IIndexWord> indexWordIterator = wordNet.getIndexWordIterator(partOfSpeech);
    int component = 1;
    while (indexWordIterator.hasNext()) {
      IIndexWord indexWord = indexWordIterator.next();
      for (IWordID wordId : indexWord.getWordIDs()) {
        LinkedList<IWord> queue = new LinkedList<IWord>();
        IWord firstWord = wordNet.getWord(wordId);
        queue.add(firstWord);
        
        while (!queue.isEmpty()) {
          IWord word = queue.removeFirst();
          ISenseKey senseKey = word.getSenseKey();
          
          if (!senseComponents.containsKey(senseKey)) {
            senseComponents.put(senseKey, component);
            
            ISenseEntry senseEntry = wordNet.getSenseEntry(senseKey);
            ISynsetID synsetId = new SynsetID(senseEntry.getOffset(), partOfSpeech);
            ISynset synset = wordNet.getSynset(synsetId);
            queue.addAll(synset.getWords());
          }
        }
        
        component++;
      }
    }
    
    // Invert map
    Map<Integer, Set<ISenseKey>> componentSenses = new TreeMap<Integer, Set<ISenseKey>>();
    for (ISenseKey senseKey : senseComponents.keySet()) {
      int componentId = senseComponents.get(senseKey);
      if (componentSenses.containsKey(componentId)) {
        componentSenses.get(componentId).add(senseKey);
      } else {
        Set<ISenseKey> senseKeys = new TreeSet<ISenseKey>();
        senseKeys.add(senseKey);
        componentSenses.put(componentId, senseKeys);
      }
    }
    
    // Get size histogram
    Map<Integer, Integer> sizeHistogram = new TreeMap<Integer, Integer>();
    for (int componentId : componentSenses.keySet()) {
      int size = componentSenses.get(componentId).size();
      if (sizeHistogram.containsKey(size)) {
        sizeHistogram.put(size, sizeHistogram.get(size) + 1);
      } else {
        sizeHistogram.put(size, 1);
      }
    }
    System.out.println("Size histogram:");
    for (int size : sizeHistogram.keySet()) {
      System.out.println("" + size + "\t" + sizeHistogram.get(size));
    }

    // Output connected components to file.
    try {
      BufferedWriter writer = new BufferedWriter(new FileWriter(
        outputPath + "connected-components.tsv"
      ));
      
      for (int componentId : componentSenses.keySet()) {
        Set<ISenseKey> senseKeys = componentSenses.get(componentId);
        for (ISenseKey senseKey : senseKeys) {
          writer.write("" + componentId + "\t" + senseKey.toString() + "\n");
        }
      }
      writer.close();
    } catch (IOException e) {
      throw new RuntimeException(e);
    }
  }

  /**
   * Runs experiments.
   */
  public void run() {
    findConnectedComponents(POS.VERB);
  }
  
  /**
   * Parses args, loads WordNet, and runs the experiments.
   */
  public static void main(String[] args) {
    String wordNetPath = args[0];
    String outputPath = args[1];
    try {
      System.out.print("Loading WordNet... ");
      IRAMDictionary wordNet = new RAMDictionary(new File(wordNetPath), ILoadPolicy.IMMEDIATE_LOAD);
      wordNet.open();
      wordNet.load(true);
      System.out.println("done.");
      WordNetGraphExperiment experiment = new WordNetGraphExperiment(wordNet, outputPath);
      experiment.run();
    } catch (Exception e) {
      throw new RuntimeException(e);
    }
  }
}
