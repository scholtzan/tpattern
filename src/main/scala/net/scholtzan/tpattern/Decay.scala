package net.scholtzan.tpattern

import org.joda.time.{Duration, Interval}

/**
  * Describes the decay of previously detected T-Patterns and input events.
  */
trait Decay {
  /**
    * Applies the defined decay to input events and returns an updated sequence of events.
    */
  def updateEvents(events: Seq[Event]): Seq[Event]

  /**
    * Applies the defined decay to detected T-Patterns and returns an updated sequence of patterns.
    */
  def updateTPatterns(patterns: Seq[TPattern]): Seq[TPattern]
}


/**
  * Events that exceed `maximumAge` will get removed.
  * TPatterns whose last occurrence is older than `maximumAge` will get removed.
  * Age is defined as the difference between the end timestamp of an event and the end timestamp of
  * the most recent event.
  */
class CutoffDecay(maximumAge: Duration) extends Decay {
  /**
    * Removes events that exceed defined age and returns updated event list.
    */
  override def updateEvents(events: Seq[Event]): Seq[Event] = {
    val mostRecent = events.maxBy(_.endTime.getMillis).endTime.getMillis
    events.filter(e => e.endTime.plusMillis(maximumAge.getMillis.toInt).getMillis - mostRecent >= 0)
  }

  /**
    * Removes T-Patterns whose occurrences exceed the defined age and returns an updated T-Pattern list.
    */
  override def updateTPatterns(patterns: Seq[TPattern]): Seq[TPattern] = {
    val mostRecent = patterns.flatMap(_.occurrences).maxBy(_._2.getMillis)._2.getMillis
    patterns.filter(p => p.occurrences.exists(_._2.plusMillis(maximumAge.getMillis.toInt).getMillis - mostRecent >= 0))
  }
}