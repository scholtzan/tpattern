package net.scholtzan.tpattern

import org.joda.time.DateTime


/**
  * Represents a temporal pattern.
  *
  * @param d1
  * @param d2
  * @param features sequence of features defining the TPattern
  * @param occurrences  occurrences of events
  */
case class TPattern (
  d1: Double,
  d2: Double,
  features: Seq[Map[String, String]],
  occurrences: Seq[(DateTime, DateTime)]
)
