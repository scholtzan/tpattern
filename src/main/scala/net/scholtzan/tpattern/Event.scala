package net.scholtzan.tpattern

import org.joda.time.DateTime

/**
  * Input events used to detect TPatterns.
  */
trait Event {
  val startTime: DateTime
  val endTime: DateTime
  val features: Map[String, String]
}
