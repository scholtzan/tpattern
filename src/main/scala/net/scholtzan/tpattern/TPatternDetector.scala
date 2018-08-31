package net.scholtzan.tpattern

import breeze.stats.distributions.Binomial
import org.joda.time.DateTime
import net.scholtzan.tpattern.utils.DateTimeUtils._

/**
  * Detects temporal patterns in data according to the algorithms described in:
  * "Discovering Hidden Temporal Patterns in Behavior and Interaction"
  */
abstract class TPatternDetector (
  private var significance: Double,
  private var minimumOccurrences: Int,
  private var subPatternDifference: Int) {


  /** Events to be analyzed. */
  protected var events: Seq[Event] = Seq()


  /**
    * Construct instance with default values.
    */
  def this() = this(0.00001, 20, 10)


  /**
    * Significance level of critical interval.
    */
  def getSignificance: Double = significance


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
    * Minimum number of occurrences of pattern so that it is considered as valid T-Pattern.
    */
  def getMinimumOccurrences: Int = minimumOccurrences


  /**
    * Sets the number of required minimum occurrences for a pattern to be a valid T-Pattern.
    * (default = 20)
    *
    * @return updated instance
    */
  def setMinimumOccurrences(minimumOccurrences: Int): this.type = {
    require(minimumOccurrences >= 0)
    this.minimumOccurrences = minimumOccurrences
    this
  }


  /**
    * Difference in number of occurrences between potential sub pattern and pattern.
    * This parameter is used to decide whether a sub pattern of another pattern. If
    * the patterns are identical however their difference in occurrences is larger
    * than this parameter, then they are considered as separate patterns. Otherwise
    * they are merged into one pattern.
    */
  def getSubpatternDifference: Int = subPatternDifference


  def setSubpatternDifference(subpatternDifference: Int): this.type = {
    require(subpatternDifference >= 0)
    this.subPatternDifference = subpatternDifference
    this
  }


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
    patterns.filter(_.occurrences.length > minimumOccurrences).groupBy(_.features).map { case (apps, tPatterns) =>
      tPatterns.head
    }.toList
  }


  protected def constructTPatterns(detectedPatterns: Seq[TPattern], newPatterns: Seq[TPattern], len: Int): Seq[TPattern] = {
    // first stage: create patterns from event types
    val patterns = constructPatterns(detectedPatterns, newPatterns)
    val tPatterns = detectTPatterns(patterns)   // todo: free, fast
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


  /** Create a table of distances between two patterns.
    *
    * @param patterns two patterns
    * @return distance table
    */
  protected def createDistanceTable(patterns: (TPattern, TPattern)): Seq[(DateTime, DateTime, Double)] = {
    // generate table with occurrences and distances between occurrences
    val tableWithDuplicates = patterns._1.occurrences.flatMap { startOccurrence =>
      patterns._2.occurrences.find(x => seconds(x._1) > seconds(startOccurrence._2)) match {
        case Some(occurrence) => Some((startOccurrence._1, occurrence._2, seconds(occurrence._1) - seconds(startOccurrence._2)))
        case _ => None
      }
    }

    // remove duplicates
    // if different A are followed by same B then they are not included in the table (only shortest)
    // also A must only occur once .groupBy(_._1).map(_._2.minBy(_._3)).toList
    tableWithDuplicates.groupBy(_._2).map(_._2.minBy(_._3)).toList.groupBy(_._1).map(_._2.minBy(_._3)).toList
  }


  /** Critical interval test. */
  protected def isCriticalInterval(d1: Double, d2: Double, nB: Double, nA: Int, nAB: Int): Boolean = {
    significance(d1, d2, nB, nA, nAB) < significance
  }


  /** Calculates the significance of two pattern occurrences. */
  protected def significance(d1: Double, d2: Double, nB: Double, nA: Int, nAB: Int): Double = {
    val pB = nB / events.length
    val d = d2 - d1

    1 - (0 until nAB).foldLeft(0.0) { (acc, i) =>
      acc + Binomial(nA, scala.math.pow(1 - pB, d)).probabilityOf(i)
    }
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
    * Detects T-Patterns.
    *
    * @param patterns potential T-Patterns
    * @return detected T-Patterns
    */
  protected def detectTPatterns(patterns: Seq[(TPattern, TPattern)]): Seq[TPattern]
}


/**
  * T-Pattern detector using the free algorithm for detection.
  */
class FreeTPatternDetector extends TPatternDetector {
  /**
    * Uses free algorithm to detect T-Patterns.
    *
    * @param patterns potential T-Patterns
    * @return detected T-Patterns
    */
  override protected def detectTPatterns(patterns: Seq[(TPattern, TPattern)]): Seq[TPattern] = {
    patterns.flatMap { pattern =>
      // create distance table
      var table = createDistanceTable(pattern).sortBy(_._3).reverse

      val nB = pattern._2.occurrences.length
      val nA = pattern._1.occurrences.length

      if (table.nonEmpty) {
        var d1 = table.head._3
        var d2 = table.last._3

        while (!isCriticalInterval(d1, d2, nB, nA, table.length) && table.nonEmpty) {
          if (significance(d1, table.last._3, nB, nA, table.length) < significance(table.head._3, d2, nB, nA, table.length)) {
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
        !isCriticalInterval(d1, d2._3, nB, nA, table.length)
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
