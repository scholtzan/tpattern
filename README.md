# Temporal Pattern Detection

> Note: This project is work in progress and currently not stable.

`tpattern` is a Scala library for detecting temporal patterns in event data. The library implements the algorithms described in: _Magnusson, Magnus S. "Discovering hidden time patterns in behavior: T-patterns and their detection." Behavior Research Methods, Instruments, & Computers 32.1 (2000): 93-110._

## Features

- [x] Free algorithm for T-Pattern detection
- [x] Fast algorithm for T-Pattern detection
- [x] Critical interval calculation based on start times and switches between events
- [ ] Detecting T-Pattern online in streaming data

## Usage

```scala
val ci = new TimeBasedCriticalIntervalMeasures()
    .setSignificance(0.0001)
val detector = new FastTPatternDetector()
    .setMinimumOccurrences(new FixedOccurrenceThreshold(2))
    .setCriticalIntervalMeasures(ci)
    .setSubPatternThreshold(0.6)
val result = detector.detect(events)
```