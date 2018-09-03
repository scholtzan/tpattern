package net.scholtzan.tpattern

import breeze.stats.distributions.Binomial
import net.scholtzan.tpattern.utils.DateTimeUtils._
import org.joda.time.DateTime

/**
  * Measures used to determine the critical interval between two T-Patterns.
  */
abstract class CriticalIntervalMeasures {
  /**
    * Configured significance threshold.
    */
  protected var significance: Double = 0.0


  /**
    * Set the significance level of the critical interval.
    * (default = 0.00001)
    *
    * @return updated instance
    */
  def setSignificance(significance: Double): this.type = {
    require(significance >= 0)
    this.significance = significance
    this
  }


  /**
    * Returns the configured significance value.
    */
  def getSignificance = this.significance


  /** Critical interval test.
    *
    * @param d1 start time unit after which second event type is starting
    * @param d2 end time unit before which second event type is starting
    * @param nB number of occurrences of second event types
    * @param nA number of occurrences of first event types
    * @param nAB  number of occurrences where first event type follows second
    * @return whether it is a critical interval
    */
  def isCriticalInterval(d1: Double, d2: Double, nB: Double, nA: Int, nAB: Int, total: Int): Boolean = {
    significance(d1, d2, nB, nA, nAB, total) < significance
  }


  /**
    * Returns the distance between two occurrences of T-Patterns as floating point number.
    */
  def distance(firstOccurrence: DateTime, secondOccurrence: DateTime, events: Seq[Event]): Double


  /**
    * Calculates the significance of two pattern occurrences.
    *
    * @param d1 start time unit after which second event type is starting
    * @param d2 end time unit before which second event type is starting
    * @param nB number of occurrences of second event types
    * @param nA number of occurrences of first event types
    * @param nAB  number of occurrences where first event type follows second
    * @param total  total number of events
    * @return significance
    */
  def significance(d1: Double, d2: Double, nB: Double, nA: Int, nAB: Int, total: Int): Double = {
    val pB = nB / total
    val d = d2 - d1 + 1

    1 - (0 until nAB).foldLeft(0.0) { (acc, i) =>
      acc + Binomial(nA, 1 - scala.math.pow(1 - pB, d)).probabilityOf(i)
    }
  }
}


/**
  * Measures for calculating the critical interval based on the starting times of T-Patterns.
  */
class TimeBasedCriticalIntervalMeasures extends CriticalIntervalMeasures {
  /**
    * Returns the time difference in seconds between the start of the first T-Pattern and
    * the start of the second T-Pattern.
    *
    * @param firstOccurrence  first T-Pattern occurrence
    * @param secondOccurrence  second T-Pattern occurrence
    */
  override def distance(firstOccurrence: DateTime, secondOccurrence: DateTime, events: Seq[Event]): Double = {
    seconds(secondOccurrence) - seconds(firstOccurrence)
  }
}


/**
  * Measures for calculating the critical interval based on switches between T-Patterns.
  *
  * @param maxSwitches  maximum number of switches between two patterns up to which they are considered as T-Pattern
  */
class SwitchBasedCriticalIntervalMeasures(protected var maxSwitches: Int) extends CriticalIntervalMeasures {
  /**
    * Returns the time difference in event switches between the start of the first T-Pattern and
    * the start of the second T-Pattern.
    *
    * @param firstOccurrence  first T-Pattern occurrence
    * @param secondOccurrence  second T-Pattern occurrence
    */
  override def distance(firstOccurrence: DateTime, secondOccurrence: DateTime, events: Seq[Event]): Double = {
    events.count(e => seconds(e.startTime) >= seconds(firstOccurrence) && seconds(e.endTime) <= seconds(secondOccurrence))
  }


  /**
    * Critical interval test.
    *
    * @param d1 start time unit after which second event type is starting
    * @param d2 end time unit before which second event type is starting
    * @param nB number of occurrences of second event types
    * @param nA number of occurrences of first event types
    * @param nAB  number of occurrences where first event type follows second
    * @param total  total number of events
    * @return whether it is a critical interval
    */
  override def isCriticalInterval(d1: Double, d2: Double, nB: Double, nA: Int, nAB: Int, total: Int): Boolean = {
    super.isCriticalInterval(d1, d2, nB, nA, nAB, total) && d2 < maxSwitches
  }
}