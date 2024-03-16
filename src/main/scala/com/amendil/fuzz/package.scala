package com.amendil

import com.amendil.entities.{CHAggregatedType, CHType}

// TODO Use this
package object fuzz {
  val fnHardcodedOutputType: Map[String, CHType] =
    Map(
      ("JSONExtract", CHAggregatedType.Any),
      ("JSONExtractKeysAndValues", CHAggregatedType.Any)
    )

  /**
    * If s1 contains "A" and "B" and s2 contains "C" and "D", combinations are considered to be:
    * (A, C)
    * (A, D)
    * (B, C)
    * (B, D)
    *
    * @param s1 List of elements
    * @param s2 List of elements
    * @return all combinations of elements between s1 and s2
    */
  private[fuzz] def crossJoin[T, U](s1: Seq[T], s2: Seq[U]): Seq[(T, U)] =
    for
      e1 <- s1
      e2 <- s2
    yield { (e1, e2) }

  /**
    * If s1 contains "A" and "B", s2 contains "C" and "D", s3 contains "E",
    * combinations are considered to be:
    * (A, C, E)
    * (A, D, E)
    * (B, C, E)
    * (B, D, E)
    *
    * @param s1 List of elements
    * @param s2 List of elements
    * @param s3 List of elements
    * @return all combinations of elements between s1, s2 and s3
    */
  private[fuzz] def crossJoin[T, U, V](s1: Seq[T], s2: Seq[U], s3: Seq[V]): Seq[(T, U, V)] =
    for
      e1 <- s1
      e2 <- s2
      e3 <- s3
    yield { (e1, e2, e3) }

  /**
    * If s1 contains "A" and "B", s2 contains "C" and "D", s3 contains "E", s4 contains "F"
    * combinations are considered to be:
    * (A, C, E, F)
    * (A, D, E, F)
    * (B, C, E, F)
    * (B, D, E, F)
    *
    * @param s1 List of elements
    * @param s2 List of elements
    * @param s3 List of elements
    * @param s4 List of elements
    * @return all combinations of elements between s1, s2, s3 and s4
    */
  private[fuzz] def crossJoin[T, U, V, W](s1: Seq[T], s2: Seq[U], s3: Seq[V], s4: Seq[W]): Seq[(T, U, V, W)] =
    for
      e1 <- s1
      e2 <- s2
      e3 <- s3
      e4 <- s4
    yield { (e1, e2, e3, e4) }

  /**
    * If s1 contains "A" and "B", s2 contains "C" and "D", s3 contains "E", s4 contains "F",
    * s5 contains "G" and "H", combinations are considered to be:
    * (A, C, E, F, G)
    * (A, C, E, F, H)
    * (A, D, E, F, G)
    * (A, D, E, F, H)
    * (B, C, E, F, G)
    * (B, C, E, F, H)
    * (B, D, E, F, G)
    * (B, D, E, F, H)
    *
    * @param s1 List of elements
    * @param s2 List of elements
    * @param s3 List of elements
    * @param s4 List of elements
    * @param s5 List of elements
    * @return all combinations of elements between s1, s2, s3, s4 and s5
    */
  private[fuzz] def crossJoin[T, U, V, W, X](
      s1: Seq[T],
      s2: Seq[U],
      s3: Seq[V],
      s4: Seq[W],
      s5: Seq[X]
  ): Seq[(T, U, V, W, X)] =
    for
      e1 <- s1
      e2 <- s2
      e3 <- s3
      e4 <- s4
      e5 <- s5
    yield { (e1, e2, e3, e4, e5) }
}
