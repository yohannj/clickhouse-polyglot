package com.amendil.bo

final case class CHResponse(
    meta: Seq[CHMetaElement],
    data: Seq[Seq[String]],
    rows: Int,
    statistics: CHResponseStatistics
)

final case class CHMetaElement(name: String, `type`: String)

final case class CHResponseStatistics(elapsed: Double, rows_read: Long, bytes_read: Long)
