package net.scholtzan.tpattern


import utest.{Tests, _}
import org.joda.time.DateTime


object CriticalIntervalMeasuresTests extends TestSuite {
  val events = Seq(Event(DateTime.parse("2018-09-02T18:46:32Z"), DateTime.parse("2018-09-02T18:46:34Z"), Map()),
                   Event(DateTime.parse("2018-09-02T18:46:34Z"), DateTime.parse("2018-09-02T18:49:32Z"), Map()),
                   Event(DateTime.parse("2018-09-02T18:49:32Z"), DateTime.parse("2018-09-02T18:50:32Z"), Map()))

  val tests = Tests{
    'changeSignificance - {
      val ci = new TimeBasedCriticalIntervalMeasures
      assert(ci.getSignificance == 0.0)
      ci.setSignificance(0.5)
      assert(ci.getSignificance == 0.5)
    }
    'invalidSignificance - {
      val ci = new TimeBasedCriticalIntervalMeasures
      intercept[IllegalArgumentException](ci.setSignificance(-0.1))
      assert(ci.getSignificance == 0.0)
    }
    'timeDistance - {
      val ci = new TimeBasedCriticalIntervalMeasures
      assert(ci.distance(DateTime.parse("2018-09-02T18:46:33Z"), DateTime.parse("2018-09-02T18:49:34Z"), events) == 181.0)
    }
    'negativeTimeDistance - {
      val ci = new TimeBasedCriticalIntervalMeasures
      assert(ci.distance(DateTime.parse("2018-09-02T18:49:34Z"), DateTime.parse("2018-09-02T18:46:33Z"), events) == -181.0)
    }
    'switchDistance - {
      val ci = new SwitchBasedCriticalIntervalMeasures(2)
      assert(ci.distance(DateTime.parse("2018-09-02T18:46:33Z"), DateTime.parse("2018-09-02T18:49:34Z"), events) == 1.0)
      assert(ci.distance(DateTime.parse("2018-09-02T18:46:32Z"), DateTime.parse("2018-09-02T18:49:34Z"), events) == 2.0)
      assert(ci.distance(DateTime.parse("2018-09-02T18:46:32Z"), DateTime.parse("2018-09-02T18:50:32Z"), events) == 3.0)
    }
    'switchBasedCriticalInterval - {
      val ci = new SwitchBasedCriticalIntervalMeasures(2)
      assert(!ci.isCriticalInterval(0, 2, 1, 1, 1, 3))  // fails because of maxSwitches
    }
    'significance - {
      val ci = new TimeBasedCriticalIntervalMeasures
      assert(ci.significance(0, 0, 1, 1, 1, 10) < 0.1)
      assert(ci.significance(0, 0, 0, 0, 0, 0) == 1.0)
      assert(ci.significance(0, 1, 100, 100, 100, 1000) < 0.00001)
    }
  }
}
