package net.scholtzan.tpattern

import org.joda.time.DateTime
import utest.{TestSuite, Tests, _}

object TPatternTests extends TestSuite {
  val pattern1 = TPattern(
    0.0, 10.0, Seq(),
    Seq((DateTime.parse("2018-09-02T18:46:32Z"), DateTime.parse("2018-09-02T18:47:32Z")),
        (DateTime.parse("2018-09-02T18:50:32Z"), DateTime.parse("2018-09-02T18:52:32Z")),
        (DateTime.parse("2018-09-02T18:55:32Z"), DateTime.parse("2018-09-02T18:57:32Z")))
  )

  val pattern2 = TPattern(
    0.0, 10.0, Seq(),
    Seq((DateTime.parse("2018-09-02T18:42:32Z"), DateTime.parse("2018-09-02T18:43:32Z")),
        (DateTime.parse("2018-09-02T18:50:30Z"), DateTime.parse("2018-09-02T18:52:50Z")),
        (DateTime.parse("2018-09-02T18:54:32Z"), DateTime.parse("2018-09-02T18:56:32Z")))
  )

  val emptyPattern = TPattern(0.0, 1.0, Seq(), Seq())

  val tests = Tests {
    'overlappingOccurrences - {
      assert(pattern1.overlappingOccurrences(pattern2).length == 2)
      assert(pattern2.overlappingOccurrences(pattern1).length == 2)
      assert(pattern2.overlappingOccurrences(pattern2).length == 3)
      assert(emptyPattern.overlappingOccurrences(pattern2).isEmpty)
      assert(pattern2.overlappingOccurrences(emptyPattern).isEmpty)
    }
  }
}