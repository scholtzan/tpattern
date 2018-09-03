package net.scholtzan.tpattern

import org.joda.time.DateTime

/**
  * Input events used to detect TPatterns.
  */
case class Event(
  startTime: DateTime,
  endTime: DateTime,
  features: Map[String, String]
)
