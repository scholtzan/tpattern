package net.scholtzan.tpattern

import org.joda.time.DateTime
import utest.{TestSuite, Tests, _}

object TPatternDetectorTests extends TestSuite {
  val events = Seq(Event(DateTime.parse("2018-09-02T18:46:32Z"), DateTime.parse("2018-09-02T18:46:33Z"), Map("name" -> "A")),
                   Event(DateTime.parse("2018-09-02T18:46:33Z"), DateTime.parse("2018-09-02T18:46:50Z"), Map("name" -> "B")),
                   Event(DateTime.parse("2018-09-02T18:46:50Z"), DateTime.parse("2018-09-02T18:46:57Z"), Map("name" -> "A")),
                   Event(DateTime.parse("2018-09-02T18:46:57Z"), DateTime.parse("2018-09-02T18:46:59Z"), Map("name" -> "B")),
                   Event(DateTime.parse("2018-09-02T18:46:59Z"), DateTime.parse("2018-09-02T18:47:11Z"), Map("name" -> "Noise")),
                   Event(DateTime.parse("2018-09-02T18:47:11Z"), DateTime.parse("2018-09-02T18:47:57Z"), Map("name" -> "A")),
                   Event(DateTime.parse("2018-09-02T18:47:57Z"), DateTime.parse("2018-09-02T18:47:59Z"), Map("name" -> "B")))

  val tests = Tests{
    'minimumOccurrences - {
      val detector = new FastTPatternDetector
      detector.setMinimumOccurrences(5)
      assert(detector.getMinimumOccurrences == 5)

      intercept[IllegalArgumentException](detector.setMinimumOccurrences(-1))
      assert(detector.getMinimumOccurrences == 5)
    }
    'subpatternThreshold - {
      val detector = new FastTPatternDetector
      detector.setSubPatternThreshold(0.12)
      assert(detector.getSubpatternThreshold == 0.12)

      intercept[IllegalArgumentException](detector.setSubPatternThreshold(-0.1))
      assert(detector.getSubpatternThreshold == 0.12)
    }
    'emptyDetection - {
      val detector = new FreeTPatternDetector
      assert(detector.detect(Seq()) == Seq())
    }
    'fastDetection - {
      val ci = new TimeBasedCriticalIntervalMeasures()
        .setSignificance(1.0)
      val detector = new FastTPatternDetector()
        .setMinimumOccurrences(2)
        .setCriticalIntervalMeasures(ci)
        .setSubPatternThreshold(0.6)
      val result = detector.detect(events)
      assert(result.length == 1)
      assert(result.forall(_.d1 == 0.0))
      assert(result.forall(_.features == List(Map("name" -> "A"), Map("name" -> "B"))))
    }
    'freeDetection - {
      val ci = new TimeBasedCriticalIntervalMeasures()
        .setSignificance(1.0)
      val detector = new FreeTPatternDetector()
        .setMinimumOccurrences(2)
        .setCriticalIntervalMeasures(ci)
        .setSubPatternThreshold(0.6)
      val result = detector.detect(events)
      assert(result.length == 1)
      assert(result.forall(_.features == List(Map("name" -> "A"), Map("name" -> "B"))))
    }
  }
}