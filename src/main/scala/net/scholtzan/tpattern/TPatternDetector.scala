package net.scholtzan.tpattern

import com.typesafe.scalalogging.LazyLogging
import net.scholtzan.tpattern.utils.DateTimeUtils._
import org.joda.time.DateTime

/**
  * Detects temporal patterns in data according to the algorithms described in:
  * "Discovering Hidden Temporal Patterns in Behavior and Interaction"
  */
abstract class TPatternDetector (
  protected var occurrenceThreshold: OccurrenceThreshold,
  protected var subPatternThreshold: Double,
  protected var criticalIntervalMeasures: CriticalIntervalMeasures
) extends LazyLogging {


  /** Events to be analyzed. */
  protected var events: Seq[Event] = Seq()


  /**
    * Construct instance with default values.
    */
  def this() = this(new FixedOccurrenceThreshold(20), 0.7, new TimeBasedCriticalIntervalMeasures)


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
    * Uses free or fast algorithm for detecting T-Patterns.
    *
    * @param events input events
    * @return detected T-Patterns
    */
  def detect(events: Seq[Event]): Seq[TPattern] = {
    this.events = events
    val trivialPatterns = constructTrivialPatterns(events)
    constructTPatterns(trivialPatterns, trivialPatterns, 1)
  }


  /**
    * Abstract method for detecting T-Patterns.
    *
    * @param patterns potential T-Patterns
    * @return detected T-Patterns
    */
  protected def detectTPatterns(patterns: Seq[(TPattern, TPattern)]): Seq[TPattern]


  /**
    * Generates all trivial patterns consisting of one event.
    *
    * @param events input events
    * @return trivial patterns the conform to completeness competition
    */
  protected def constructTrivialPatterns(events: Seq[Event]): Seq[TPattern] = {
    val trivialPatterns = events.groupBy(_.features).map { case (features, correspondingEvents) =>
      TPattern(0, 0, Seq(features), correspondingEvents.map(e => (e.startTime, e.endTime)))
    }.toList

    val completeTrivialPatterns = completenessCompetition(trivialPatterns)

    constructTPatterns(completeTrivialPatterns, completeTrivialPatterns, 1)
  }


  /**
    * Redundant or partial detections of the same underlying patterns are dropped.
    *
    * @param patterns TPatterns checked for completeness
    * @return TPatterns that fulfill the completeness competition
    */
  protected def completenessCompetition(patterns: Seq[TPattern]): Seq[TPattern] = {
    // remove redundant patterns
    val distinctPatterns = patterns.filter(x => occurrenceThreshold.exceeded(x.occurrences.length, events.length)).distinct
    mergePatterns(distinctPatterns)
  }


  /** Merges patterns that are sub patterns of another pattern into it based on [todo].
    *
    * @param patterns patterns to be merged
    * @return merged patterns
    */
  private def mergePatterns(patterns: Seq[TPattern]): Seq[TPattern] = {
    patterns.flatMap { pattern =>
      // maximum number of occurrences that pattern is sub pattern of another pattern
      val maxOverlap = patterns.map { parent =>
        if (pattern == parent) {
          0
        } else if (isSubPattern(parent, pattern)) {
          pattern.overlappingOccurrences(parent).length
        } else {
          0
        }
      }.max.toDouble / pattern.occurrences.length.toDouble

      if (maxOverlap >= subPatternThreshold) {
        None
      } else {
        Some(pattern)
      }
    }
  }


  /**
    * Determine T-Patterns from previously detected and generated patterns.
    *
    * @param detectedPatterns previously detected T-Patterns
    * @param newPatterns  newly generated patterns
    * @param len  length of the previously detect T-Patterns
    * @return generated T-Patterns
    */
  protected def constructTPatterns(detectedPatterns: Seq[TPattern], newPatterns: Seq[TPattern], len: Int): Seq[TPattern] = {
    // first stage: create patterns from event types
    val patterns = constructPatterns(detectedPatterns, newPatterns)
    val tPatterns = detectTPatterns(patterns)
    val validatedTPatterns = completenessCompetition(detectedPatterns ++ tPatterns)
    val newDetectedPatterns = validatedTPatterns.filter(_.features.length == len + 1)

    if (newDetectedPatterns.isEmpty) {
      return validatedTPatterns
    }

    constructTPatterns(validatedTPatterns, newDetectedPatterns, len + 1)
  }


  /**
    * Based on already detected patterns, construct the next patterns by merging them.
    *
    * @param previousPatterns   previously detected patterns
    * @param latestPatterns   most recently detected patterns
    * @return   list of previously detected patterns and the newly created patterns
    */
  protected def constructPatterns(previousPatterns: Seq[TPattern], latestPatterns: Seq[TPattern]): Seq[(TPattern, TPattern)] = {
    latestPatterns.flatMap(latestPattern => previousPatterns.flatMap { previousPattern =>
      if (previousPattern != latestPattern) {
        if (previousPattern.features.last != latestPattern.features.head && previousPattern.features.head != latestPattern.features.last) {
          Some(List((previousPattern, latestPattern), (latestPattern, previousPattern)))
        } else if (previousPattern.features.last == latestPattern.features.head && previousPattern.features.head != latestPattern.features.last) {
          Some(List((latestPattern, previousPattern)))
        } else if (previousPattern.features.last != latestPattern.features.head && previousPattern.features.head == latestPattern.features.last) {
          Some(List((previousPattern, latestPattern)))
        } else {
          None
        }
      } else {
        None
      }
    }.flatten)
  }


  /**
    * Create a table of distances between two patterns.
    *
    * @param patterns two patterns
    * @return distance table
    */
  protected def createDistanceTable(patterns: (TPattern, TPattern)): Seq[(DateTime, DateTime, Double)] = {
    // generate table with occurrences and distances between occurrences
    val tableWithDuplicates = patterns._1.occurrences.flatMap { startOccurrence =>
      patterns._2.occurrences.find(x => seconds(x._1) > seconds(startOccurrence._1)) match {
        case Some(occurrence) => Some((startOccurrence._1, occurrence._2, criticalIntervalMeasures.distance(startOccurrence._2, occurrence._1, events)))
        case _ => None
      }
    }

    // remove duplicates
    // if different A are followed by same B then they are not included in the table (only shortest)
    // also A must only occur once
    tableWithDuplicates.groupBy(_._2).map(_._2.minBy(_._3)).toList.groupBy(_._1).map(_._2.minBy(_._3)).toList
  }


  /**
    *  Checks whether `subPattern` is contained in `pattern`.
    */
  protected def isSubPattern(pattern: TPattern, subPattern: TPattern): Boolean = {
    pattern.features.containsSlice(subPattern.features)
  }
}


/**
  * T-Pattern detector using the free algorithm for detection.
  */
class FreeTPatternDetector() extends TPatternDetector {
  /**
    * Uses free algorithm to detect T-Patterns.
    *
    * @param patterns potential T-Patterns
    * @return detected T-Patterns
    */
  override protected def detectTPatterns(patterns: Seq[(TPattern, TPattern)]): Seq[TPattern] = {
    patterns.flatMap { pattern =>
      // create distance table
      var table = createDistanceTable(pattern).sortBy(_._3)

      val nB = pattern._2.occurrences.length
      val nA = pattern._1.occurrences.length

      if (table.nonEmpty) {
        var d1 = table.head._3
        var d2 = table.last._3

        while (!criticalIntervalMeasures.isCriticalInterval(d1, d2, nB, nA, table.length, events.length) && table.nonEmpty && d1 < d2) {
          if (criticalIntervalMeasures.significance(d1, table.last._3, nB, nA, table.length, events.length) <
              criticalIntervalMeasures.significance(table.head._3, d2, nB, nA, table.length, events.length)) {
            d2 = table.last._3
            table = table.init
          } else {
            d1 = table.head._3
            table = table.tail
          }
        }
      }

      val significantPatterns = table

      if (significantPatterns.nonEmpty) {
        Some(TPattern(
          significantPatterns.head._3,
          significantPatterns.last._3, // first significant distance
          pattern._1.features ++ pattern._2.features,
          significantPatterns.map(x => (x._1, x._2))
        ))
      } else {
        None
      }
    }
  }
}


/**
  * T-Pattern detector using the fast algorithm for detection.
  */
class FastTPatternDetector extends TPatternDetector {
  /**
    * Uses fast algorithm to detect T-Patterns.
    *
    * @param patterns potential T-Patterns
    * @return detected T-Patterns
    */
  override protected def detectTPatterns(patterns: Seq[(TPattern, TPattern)]): Seq[TPattern] = {
    patterns.flatMap { pattern =>
      // create distance table
      val table = createDistanceTable(pattern).sortBy(_._3).reverse
      val d1 = 0
      val nB = pattern._2.occurrences.length
      val nA = pattern._1.occurrences.length

      // determine significant patterns
      val insignificant = table.takeWhile(d2 =>
        !criticalIntervalMeasures.isCriticalInterval(d1, d2._3, nB, nA, table.length, events.length)
      )

      val significantPatterns = table.drop(insignificant.length)

      // get T-Patterns
      if (significantPatterns.nonEmpty) {
        Some(TPattern(
          d1,
          significantPatterns.head._3, // first significant distance
          pattern._1.features ++ pattern._2.features,
          significantPatterns.map(x => (x._1, x._2))
        ))
      } else {
        None
      }
    }
  }
}
