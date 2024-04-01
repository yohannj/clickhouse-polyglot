package com.amendil.common.entities

final case class CHResponse(
    meta: Seq[CHMetaElement],
    data: Seq[Seq[?]],
    rows: Int,
    statistics: CHResponseStatistics
)

final case class CHMetaElement(name: String, `type`: String)

final case class CHResponseStatistics(elapsed: Double, rows_read: Long, bytes_read: Long)
