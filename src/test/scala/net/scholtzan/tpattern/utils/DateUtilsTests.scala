package net.scholtzan.tpattern.utils

import utest._
import DateTimeUtils._
import org.joda.time.DateTime
import org.joda.time.format.DateTimeFormat

object DateUtilsTests extends TestSuite {
  val tests = Tests{
    'seconds - {
      val date = DateTime.parse("2018-09-02T18:46:32Z")
      assert(seconds(date) == 1535913992.0)
    }
  }
}