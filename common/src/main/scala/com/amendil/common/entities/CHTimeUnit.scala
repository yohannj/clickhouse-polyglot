package com.amendil.common.entities

// Alias found in: https://clickhouse.com/docs/en/sql-reference/functions/date-time-functions#date_diff

/**
  * Interval unit that have a precision only seen in DateTime64
  *
  * @param values alias that ClickHouse uses to specify the unit
  */
enum CHTime64Unit(val values: Seq[String]):
  case Nanosecond extends CHTime64Unit(Seq("nanosecond", "nanoseconds", "ns"))
  case Microsecond extends CHTime64Unit(Seq("microsecond", "microseconds", "us", "u"))
  case Millisecond extends CHTime64Unit(Seq("millisecond", "milliseconds", "ms"))

/**
  * Interval unit that have a precision only seen in DateTime and DateTime64
  *
  * @param values alias that ClickHouse uses to specify the unit
  */
enum CHTimeUnit(val values: Seq[String]):
  case Second extends CHTimeUnit(Seq("second", "seconds", "ss", "s"))
  case Minute extends CHTimeUnit(Seq("minute", "minutes", "mi", "n"))
  case Hour extends CHTimeUnit(Seq("hour", "hours", "hh", "h"))

/**
  * Interval unit that have a precision seen in all kinds of Date types
  *
  * @param values alias that ClickHouse uses to specify the unit
  */
enum CHDateUnit(val values: Seq[String]):
  case Day extends CHDateUnit(Seq("day", "days", "dd", "d"))
  case Week extends CHDateUnit(Seq("week", "weeks", "wk", "ww"))
  case Month extends CHDateUnit(Seq("month", "months", "mm", "m"))
  case Quarter extends CHDateUnit(Seq("quarter", "quarters", "qq", "q"))
  case Year extends CHDateUnit(Seq("year", "years", "yyyy", "yy"))
