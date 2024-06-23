package com.amendil.common.entities

/**
  * CHResponse represents the response of a call going through ClickHouse's HTTP interface when the
  * auery that was sent used `FORMAT JSONCompact`
  *
  * @param meta Header of the data in the response (Column name and types)
  * @param data List of rows, each row being a list of columns
  * @param rows Number of rows in the response
  * @param statistics Statistics about the query execution in ClickHouse
  */
final case class CHResponse(
    meta: Seq[CHMetaElement],
    data: Seq[Seq[?]],
    rows: Int,
    statistics: CHResponseStatistics
)

final case class CHMetaElement(name: String, `type`: String)

final case class CHResponseStatistics(elapsed: Double, rows_read: Long, bytes_read: Long)
