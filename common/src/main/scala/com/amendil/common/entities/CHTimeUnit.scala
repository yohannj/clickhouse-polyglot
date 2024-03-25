package com.amendil.common.entities

enum CHTimeUnit(val values: Seq[String]) {
  case Microsecond extends CHTimeUnit(Seq("microsecond", "microseconds", "us", "u"))
  case Millisecond extends CHTimeUnit(Seq("millisecond", "milliseconds", "ms"))
  case Second extends CHTimeUnit(Seq("second", "seconds", "ss", "s"))
  case Minute extends CHTimeUnit(Seq("minute", "minutes", "mi", "n"))
  case Hour extends CHTimeUnit(Seq("hour", "hours", "hh", "h"))
}

enum CHDateUnit(val values: Seq[String]) {
  case Day extends CHDateUnit(Seq("day", "days", "dd", "d"))
  case Week extends CHDateUnit(Seq("week", "weeks", "wk", "ww"))
  case Month extends CHDateUnit(Seq("month", "months", "mm", "m"))
  case Quarter extends CHDateUnit(Seq("quarter", "quarters", "qq", "q"))
  case Year extends CHDateUnit(Seq("year", "years", "yyyy", "yy"))
}
