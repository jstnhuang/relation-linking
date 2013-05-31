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
import edu.knowitall.openie.models.serialize.Chill

class SolrQueryExecutor (solrUrl: String) extends QueryExecutor {
  val solr = new HttpSolrServer(solrUrl)
  val kryo = Chill.createInjection()
  
  /**
   * Query the Solr DB and return the results as an iterator.
   */
  def execute(queryString: String): Iterator[ExtractionGroup[ReVerbExtraction]] = {
    val squery = new SolrQuery()
    squery.setQuery(queryString)
    squery.setRows(100000)
    val response = solr.query(squery)
    for (result <- response.getResults().iterator().asScala) yield {
      convertSolrDocument(result)
    }
  }
  
  /**
   * Converts a Solr document into a ReVerb extraction group.
   */
  def convertSolrDocument(solrDocument: SolrDocument): ExtractionGroup[ReVerbExtraction] = {
    val bytes = solrDocument.getFieldValue("instances").asInstanceOf[Array[Byte]]
    val instances: List[Instance[ReVerbExtraction]] =
      kryo.invert(bytes).getOrElse(
        throw new IllegalArgumentException(
          "Could not deserialize instances: " + bytes.toSeq.toString
        )
      ).asInstanceOf[List[Instance[ReVerbExtraction]]]
    
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
      srlLink = {
        if (!solrDocument.containsKey("srl_link")) {
          None
        } else {
          Some(solrDocument.getFieldValue("srl_link").asInstanceOf[String])
        }
      },
      wnLink = {
        if (!solrDocument.containsKey("wn_link")) {
          None
        } else {
          Some(solrDocument.getFieldValue("wn_link").asInstanceOf[String])
        }
      },
      vnLinks = {
        if (!solrDocument.containsKey("vn_links")) {
          Set.empty[String]
        } else {
          solrDocument.getFieldValue("vn_links").asInstanceOf[java.util.List[String]].asScala.toSet
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
