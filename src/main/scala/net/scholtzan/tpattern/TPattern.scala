package net.scholtzan.tpattern

import org.joda.time.DateTime
import net.scholtzan.tpattern.utils.DateTimeUtils._

/**
  * Represents a temporal pattern.
  *
  * @param d1 start time unit after which next pattern starts
  * @param d2 end time unit before which next pattern starts
  * @param features sequence of features defining the TPattern
  * @param occurrences  occurrences of events
  */
case class TPattern (
  d1: Double,
  d2: Double,
  features: Seq[Map[String, String]],
  occurrences: Seq[(DateTime, DateTime)]
) {
  /** Checks if the T-Pattern is overlapping with another provided pattern based
    * on their occurrences and returns the overlapping occurrences.
    *
    * @param pattern potentially overlapping pattern
    * @return time segments that overlap
    */
  def overlappingOccurrences(pattern: TPattern): Seq[(DateTime, DateTime)] = {
    occurrences.filter { occ =>
      pattern.occurrences.exists(o => (seconds(o._1) <= seconds(occ._1) && seconds(o._2) > seconds(occ._1)) ||
        (seconds(o._1) < seconds(occ._2) && seconds(o._2) >= seconds(occ._2)) ||
        (seconds(o._1) >= seconds(occ._1) && seconds(o._2) <= seconds(occ._2)) ||
        (seconds(o._1) <= seconds(occ._1) && seconds(o._2) >= seconds(occ._2)))
    }
  }
}
