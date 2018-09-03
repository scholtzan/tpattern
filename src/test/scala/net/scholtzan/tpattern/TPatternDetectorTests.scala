package net.scholtzan.tpattern

import utest.{TestSuite, Tests, _}

object TPatternDetectorTests extends TestSuite {
  val tests = Tests{
    'significance - {
      val detector = new FastTPatternDetector
      detector.setSignificance(0.23)
      assert(detector.getSignificance == 0.23)

      intercept[IllegalArgumentException](detector.setSignificance(-0.1))
      assert(detector.getSignificance == 0.23)
    }
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
    'criticalIntervalMeasures - {
      val detector = new FastTPatternDetector
      val ci = new TimeBasedCriticalIntervalMeasures
      ci.setSignificance(0.14)
      detector.setCriticalIntervalMeasures(ci)
      detector.setSignificance(0.34)
      assert(detector.getSignificance == 0.34)
    }
    'emptyDetection - {
      val detector = new FreeTPatternDetector
      assert(detector.detect(Seq()) == Seq())
    }
    'fastDetection - {
      // todo
      assert(true)
    }
    'freeDetection - {
      // todo
      assert(true)
    }
  }
}