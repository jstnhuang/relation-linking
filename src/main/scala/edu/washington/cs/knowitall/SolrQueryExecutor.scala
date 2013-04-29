package edu.washington.cs.knowitall

import scala.collection.Iterator
import edu.knowitall.openie.models.ExtractionGroup
import edu.knowitall.openie.models.ReVerbExtraction
import edu.knowitall.openie.models.ExtractionArgument
import edu.knowitall.openie.models.ExtractionRelation
import org.apache.solr.common.SolrDocument
import org.apache.solr.client.solrj.impl.HttpSolrServer
import edu.knowitall.openie.models.FreeBaseEntity
import edu.knowitall.common.Resource.using
import scala.collection.JavaConverters._
import java.io.ByteArrayInputStream
import edu.knowitall.openie.models.FreeBaseType
import java.io.ObjectInputStream
import edu.knowitall.openie.models.Instance
import org.apache.solr.client.solrj.SolrQuery

class SolrQueryExecutor (solrUrl: String) extends QueryExecutor {
  val solr = new HttpSolrServer(solrUrl)
  
  /**
   * Query the Solr DB and return the results as an iterator.
   */
  def execute(queryString: String): Iterator[ExtractionGroup[ReVerbExtraction]] = {
    val squery = new SolrQuery()
    squery.setQuery(queryString)
    squery.setRows(1000)
    val response = solr.query(squery)
    for (result <- response.getResults().iterator().asScala) yield {
      convertSolrDocument(result)
    }
  }
  
  /**
   * Converts a Solr document into a ReVerb extraction group.
   */
  def convertSolrDocument(solrDocument: SolrDocument): ExtractionGroup[ReVerbExtraction] = {
    val instances = using(new ByteArrayInputStream(
        solrDocument.getFieldValue("instances").asInstanceOf[Array[Byte]])) { is =>
      using(new ObjectInputStream(is) {
        override def resolveClass(desc: java.io.ObjectStreamClass): Class[_] = {
          try { Class.forName(desc.getName, false, getClass.getClassLoader) }
          catch { case ex: ClassNotFoundException => super.resolveClass(desc) }
        }
      }) { ois =>
        ois.readObject().asInstanceOf[List[Instance[ReVerbExtraction]]]
      }
    }
    
    val arg1 = ExtractionArgument(
      norm = solrDocument.getFieldValue("arg1").asInstanceOf[String],
      entity = {
        if (!solrDocument.containsKey("arg1_entity_id")) None
        else {
          val id = solrDocument.getFieldValue("arg1_entity_id").asInstanceOf[String]
          val name = solrDocument.getFieldValue("arg1_entity_name").asInstanceOf[String]
          val inlink_ratio = solrDocument.getFieldValue("arg1_entity_inlink_ratio")
            .asInstanceOf[Double]
          val score = solrDocument.getFieldValue("arg1_entity_score").asInstanceOf[Double]
          Some(FreeBaseEntity(name, id, score, inlink_ratio))
        }
      },
      types = {
        if (!solrDocument.containsKey("arg1_fulltypes")) { Set.empty }
        else {
          solrDocument.getFieldValue("arg1_fulltypes").asInstanceOf[java.util.List[String]]
            .asScala.map(FreeBaseType.parse(_).get).toSet
        }
      }
    )

    val arg2 = ExtractionArgument(
      norm = solrDocument.getFieldValue("arg2").asInstanceOf[String],
      entity = {
        if (!solrDocument.containsKey("arg2_entity_id")) None
        else {
          val id = solrDocument.getFieldValue("arg2_entity_id").asInstanceOf[String]
          val name = solrDocument.getFieldValue("arg2_entity_name").asInstanceOf[String]
          val inlink_ratio = solrDocument.getFieldValue("arg2_entity_inlink_ratio")
            .asInstanceOf[Double]
          val score = solrDocument.getFieldValue("arg2_entity_score").asInstanceOf[Double]
          Some(FreeBaseEntity(name, id, score, inlink_ratio))
        }
      },
      types = {
        if (!solrDocument.containsKey("arg2_fulltypes")) { Set.empty }
        else { solrDocument.getFieldValue("arg2_fulltypes").asInstanceOf[java.util.List[String]]
          .asScala.map(FreeBaseType.parse(_).get).toSet
        }
      }
    )

    val rel = ExtractionRelation(
      norm = solrDocument.getFieldValue("rel").asInstanceOf[String],
      link = {
        if (!solrDocument.containsKey("rel_link_id")) {
          None
        } else {
          Some(solrDocument.getFieldValue("rel_link_id").asInstanceOf[String])
        }
      }
    )

    ExtractionGroup[ReVerbExtraction](
      arg1 = arg1,
      rel  = rel,
      arg2 = arg2,
      instances = instances.toSet
    )
  }
}
