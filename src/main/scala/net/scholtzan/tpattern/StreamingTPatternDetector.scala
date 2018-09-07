package net.scholtzan.tpattern

import com.typesafe.scalalogging.LazyLogging

/**
  * T-Pattern detection on streaming data.
  * todo: sub class of `TPatternDetector`?
  */
class StreamingTPatternDetector (
  protected var occurrenceThreshold: OccurrenceThreshold,
  protected var subPatternThreshold: Double,
  protected var criticalIntervalMeasures: CriticalIntervalMeasures,
  protected var decayFactor: Double
) extends LazyLogging {
  /** Events to be analyzed. */
  protected var events: Seq[Event] = Seq()


  /**
    * Construct instance with default values.
    */
  def this() = this(new FixedOccurrenceThreshold(20), 0.7, new TimeBasedCriticalIntervalMeasures, 0.2)


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
  def getDecayFactor: Double = decayFactor


  /** Sets the decay factor for detected T-Patterns.
    * (default = 0.2)
    *
    * @return updated instance
    */
  def setDecayFactor(decayFactor: Double): this.type = {
    require(decayFactor >= 0.0)
    this.decayFactor = decayFactor
    this
  }
}
