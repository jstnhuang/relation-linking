import AssemblyKeys._

assemblySettings

organization := "edu.washington.cs.knowitall"

name := "relation-linking"

description := "Relation linking and entailment code."

version := "1.0-SNAPSHOT"

crossScalaVersions := Seq("2.10.0", "2.9.2")

libraryDependencies ++= Seq(
  "edu.mit" % "jwi" % "2.2.3",
  "com.nicta" %% "scoobi" % "0.6.0-cdh3",
  "com.github.scopt" %% "scopt" % "2.1.0",
  "edu.washington.cs.knowitall.openie" % "openie-models_2.9.3" % "1.0.0-SNAPSHOT",
  "edu.washington.cs.knowitall.nlptools" %% "nlptools-srl-clear" % "2.4.1",
  "edu.washington.cs.knowitall.nlptools" %% "nlptools-postag-opennlp" % "2.4.1",
  "edu.washington.cs.knowitall.nlptools" %% "nlptools-tokenize-opennlp" % "2.4.1",
  "org.apache.derby" % "derby" % "10.9.1.0",
  "org.apache.derby" % "derbyclient" % "10.9.1.0",
  "org.apache.solr" % "solr-solrj" % "4.0.0",
  "ch.qos.logback" % "logback-classic" % "1.0.12",
  "ch.qos.logback" % "logback-core" % "1.0.12",
  "commons-logging" % "commons-logging-api" % "1.0.4",
  "org.slf4j" % "slf4j-api" % "1.7.2"
)

resolvers ++= Seq(
  "nicta" at "http://nicta.github.com/scoobi/releases",
  "cloudera" at "https://repository.cloudera.com/content/repositories/releases"
)

mainClass in assembly := Some("edu.washington.cs.knowitall.browser.hadoop.scoobi.ReverbRelationLinker")

mergeStrategy in assembly <<= (mergeStrategy in assembly) { (old) =>
  {
    case PathList("javax", "servlet", xs @ _*) => MergeStrategy.first
    case PathList("org", "xmlpull", xs @ _*) => MergeStrategy.first
    case x => old(x)
  }
}
