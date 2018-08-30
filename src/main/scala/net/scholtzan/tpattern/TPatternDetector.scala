package net.scholtzan.tpattern

/**
  * Detects temporal patterns in data according to the algorithms described in:
  * "Discovering Hidden Temporal Patterns in Behavior and Interaction"
  */
class TPatternDetector (
  private var significance: Double,
  private var minimumOccurrences: Int,
  private var subPatternDifference: Int) {

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
}
