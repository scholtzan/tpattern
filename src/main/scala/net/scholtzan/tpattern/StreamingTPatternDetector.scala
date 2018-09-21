package net.scholtzan.tpattern

import com.typesafe.scalalogging.LazyLogging
import org.joda.time.Duration

/**
  * T-Pattern detection on streaming data.
  * todo: sub class of `TPatternDetector`?
  */
class StreamingTPatternDetector (
  protected var occurrenceThreshold: OccurrenceThreshold,
  protected var subPatternThreshold: Double,
  protected var criticalIntervalMeasures: CriticalIntervalMeasures,
  protected var decay: Decay
) extends LazyLogging {

  /** Events to be analyzed. */
  protected var events: Seq[Event] = Seq()


  /** Tracks the detected T-Patterns. */
  protected var detectedPatterns: Seq[TPattern] = Seq()


  /**
    * Construct instance with default values.
    */
  def this() = this(new FixedOccurrenceThreshold(20), 0.7, new TimeBasedCriticalIntervalMeasures, new CutoffDecay(new Duration(60 * 60 * 1000)))


  /**
    * Minimum number of occurrences of pattern so that it is considered as valid T-Pattern.
    */
  def getMinimumOccurrences: OccurrenceThreshold = occurrenceThreshold


  /**
    * Sets the threshold required minimum occurrences for a pattern to be a valid T-Pattern.
    * (default = FixedOccurrenceThreshold(20))
    *
    * @return updated instance
    */
  def setOccurrenceThreshold(occurrenceThreshold: OccurrenceThreshold): this.type = {
    this.occurrenceThreshold = occurrenceThreshold
    this
  }


  /**
    * Threshold that sub pattern needs to exceed to be considered as separate pattern.
    * This threshold is defined for a pattern A that is sup pattern of B as:
    * number of occurrences of A overlapping with B / total occurrences of A
    */
  def getSubpatternThreshold: Double = subPatternThreshold


  /** Sets the minimum threshold for a sub pattern to be considered as separate pattern.
    * (default = 0.7)
    *
    * @return updated instance
    */
  def setSubPatternThreshold(subPatternThreshold: Double): this.type = {
    require(subPatternThreshold >= 0.0 && subPatternThreshold <= 1.0)
    this.subPatternThreshold = subPatternThreshold
    this
  }


  /**
    * Sets the critical interval measures used to determine the critical interval.
    * (default = `TimeBasedCriticalIntervalMeasures`)
    *
    * @return updated instance
    */
  def setCriticalIntervalMeasures(ciMeasures: CriticalIntervalMeasures): this.type = {
    this.criticalIntervalMeasures = ciMeasures
    this
  }


  /**
    * Scales the contribution of the T-Patterns that have been detected so far.
    */
  def getDecay: Decay = decay


  /** Sets the decay for detected T-Patterns.
    * (default = CutoffDecay(1 hour))
    *
    * @return updated instance
    */
  def setDecay(decay: Decay): this.type = {
    this.decay = decay
    this
  }


  /**
    * Update the detected T-Patterns by taking new events and decay factors into account.
    * @param newData  newly received events
    */
  def update(newData: Seq[Event]) = {
    // todo: currently very naive implementation in which all T-Patterns are recalculated for each new event set
    val patternDetector = new FastTPatternDetector()
      .setCriticalIntervalMeasures(this.criticalIntervalMeasures)
      .setOccurrenceThreshold(this.occurrenceThreshold)
      .setSubPatternThreshold(this.subPatternThreshold)

    this.detectedPatterns = patternDetector.detect(this.decay.updateEvents(this.events))
  }
}
