package net.scholtzan.tpattern

/**
  * Determines whether there are enough occurrences of a pattern to be considered as T-Pattern.
  */
trait OccurrenceThreshold {
  /**
    * Checks whether a certain threshold is exceeded.
    */
  def exceeded(occurrences: Int, total: Int): Boolean
}


/**
  * Threshold defined as a constant number.
  */
class FixedOccurrenceThreshold(threshold: Int) extends OccurrenceThreshold {
  /**
    * Checks whether the number of occurrences exceeds a defined constant.
    */
  override def exceeded(occurrences: Int, total: Int): Boolean = {
    occurrences > threshold
  }
}


/**
  * Threshold defined as `ratio` * `total`.
  */
class LinearOccurrenceThreshold(ratio: Double) extends OccurrenceThreshold {
  /**
    * Checks whether the number of occurrences exceeds `ratio` * `total`.
    */
  override def exceeded(occurrences: Int, total: Int): Boolean = {
    occurrences > total * ratio
  }
}