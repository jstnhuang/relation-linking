organization := "edu.washington.cs.knowitall"

name := "relation-linking"

description := "Relation linking and entailment code."

version := "1.0-SNAPSHOT"

crossScalaVersions := Seq("2.10.0", "2.9.2")

libraryDependencies ++= Seq(
  "edu.mit" % "jwi" % "2.2.3",
  "com.nicta" %% "scoobi" % "0.5.0-cdh3",
  "com.github.scopt" %% "scopt" % "2.1.0",
  "edu.washington.cs.knowitall.openie" % "openie-models_2.9.3" % "1.0.0-SNAPSHOT",
  "edu.washington.cs.knowitall.nlptools" %% "nlptools-srl-clear" % "2.4.1",
  "edu.washington.cs.knowitall.nlptools" %% "nlptools-postag-opennlp" % "2.4.1",
  "edu.washington.cs.knowitall.nlptools" %% "nlptools-tokenize-opennlp" % "2.4.1",
  "org.apache.derby" % "derby" % "10.9.1.0",
  "org.apache.solr" % "solr-solrj" % "4.0.0"
)

resolvers ++= Seq(
  "nicta" at "http://nicta.github.com/scoobi/releases",
  "cloudera" at "https://repository.cloudera.com/content/repositories/releases"
)
