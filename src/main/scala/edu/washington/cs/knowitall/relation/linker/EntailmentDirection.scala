package edu.washington.cs.knowitall.relation.linker

object EntailmentDirection extends Enumeration {
  type EntailmentDirection = Value
  val Hypernym, Troponym = Value
}