package net.scholtzan.tpattern.utils

import org.joda.time.DateTime


/**
  * Contains helper functions related to `DateTime`.
  */
object DateTimeUtils {
  /** Converts a DateTime object into seconds. */
  def seconds(time: DateTime): Double = {
    time.getMillis / 1000.0
  }
}