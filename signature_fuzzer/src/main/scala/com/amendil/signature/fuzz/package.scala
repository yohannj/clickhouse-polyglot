package com.amendil.signature

package object fuzz:

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
    yield (e1, e2)

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
    yield (e1, e2, e3)

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
    yield (e1, e2, e3, e4)

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
    yield (e1, e2, e3, e4, e5)

  /**
    * If s1 contains "A" and "B", s2 contains "C" and "D", s3 contains "E", s4 contains "F",
    * s5 contains "G" and "H", s6 contains "I" and "J" combinations are considered to be:
    * (A, C, E, F, G, I)
    * (A, C, E, F, H, I)
    * (A, D, E, F, G, I)
    * (A, D, E, F, H, I)
    * (B, C, E, F, G, I)
    * (B, C, E, F, H, I)
    * (B, D, E, F, G, I)
    * (B, D, E, F, H, I)
    * (A, C, E, F, G, J)
    * (A, C, E, F, H, J)
    * (A, D, E, F, G, J)
    * (A, D, E, F, H, J)
    * (B, C, E, F, G, J)
    * (B, C, E, F, H, J)
    * (B, D, E, F, G, J)
    * (B, D, E, F, H, J)
    *
    * @param s1 List of elements
    * @param s2 List of elements
    * @param s3 List of elements
    * @param s4 List of elements
    * @param s5 List of elements
    * @param s6 List of elements
    * @return all combinations of elements between s1, s2, s3, s4, s5 and s6
    */
  private[fuzz] def crossJoin[T, U, V, W, X, Y](
      s1: Seq[T],
      s2: Seq[U],
      s3: Seq[V],
      s4: Seq[W],
      s5: Seq[X],
      s6: Seq[Y]
  ): Seq[(T, U, V, W, X, Y)] =
    for
      e1 <- s1
      e2 <- s2
      e3 <- s3
      e4 <- s4
      e5 <- s5
      e6 <- s6
    yield (e1, e2, e3, e4, e5, e6)

  /**
    * See doc of crossJoin with less parameters for more information
    *
    * @param s1 List of elements
    * @param s2 List of elements
    * @param s3 List of elements
    * @param s4 List of elements
    * @param s5 List of elements
    * @param s6 List of elements
    * @param s7 List of elements
    * @return all combinations of elements between s1, s2, s3, s4, s5, s6 and s7
    */
  private[fuzz] def crossJoin[T, U, V, W, X, Y, Z](
      s1: Seq[T],
      s2: Seq[U],
      s3: Seq[V],
      s4: Seq[W],
      s5: Seq[X],
      s6: Seq[Y],
      s7: Seq[Z]
  ): Seq[(T, U, V, W, X, Y, Z)] =
    for
      e1 <- s1
      e2 <- s2
      e3 <- s3
      e4 <- s4
      e5 <- s5
      e6 <- s6
      e7 <- s7
    yield (e1, e2, e3, e4, e5, e6, e7)

  /**
    * See doc of crossJoin with less parameters for more information
    *
    * @param s1 List of elements
    * @param s2 List of elements
    * @param s3 List of elements
    * @param s4 List of elements
    * @param s5 List of elements
    * @param s6 List of elements
    * @param s7 List of elements
    * @param s8 List of elements
    * @return all combinations of elements between s1, s2, s3, s4, s5, s6, s7 and s8
    */
  private[fuzz] def crossJoin[T, U, V, W, X, Y, Z, ZA](
      s1: Seq[T],
      s2: Seq[U],
      s3: Seq[V],
      s4: Seq[W],
      s5: Seq[X],
      s6: Seq[Y],
      s7: Seq[Z],
      s8: Seq[ZA]
  ): Seq[(T, U, V, W, X, Y, Z, ZA)] =
    for
      e1 <- s1
      e2 <- s2
      e3 <- s3
      e4 <- s4
      e5 <- s5
      e6 <- s6
      e7 <- s7
      e8 <- s8
    yield (e1, e2, e3, e4, e5, e6, e7, e8)

  /**
    * See doc of crossJoin with less parameters for more information
    *
    * @param s1 List of elements
    * @param s2 List of elements
    * @param s3 List of elements
    * @param s4 List of elements
    * @param s5 List of elements
    * @param s6 List of elements
    * @param s7 List of elements
    * @param s8 List of elements
    * @param s9 List of elements
    * @return all combinations of elements between s1, s2, s3, s4, s5, s6, s7, s8 and s9
    */
  private[fuzz] def crossJoin[T, U, V, W, X, Y, Z, ZA, ZB](
      s1: Seq[T],
      s2: Seq[U],
      s3: Seq[V],
      s4: Seq[W],
      s5: Seq[X],
      s6: Seq[Y],
      s7: Seq[Z],
      s8: Seq[ZA],
      s9: Seq[ZB]
  ): Seq[(T, U, V, W, X, Y, Z, ZA, ZB)] =
    for
      e1 <- s1
      e2 <- s2
      e3 <- s3
      e4 <- s4
      e5 <- s5
      e6 <- s6
      e7 <- s7
      e8 <- s8
      e9 <- s9
    yield (e1, e2, e3, e4, e5, e6, e7, e8, e9)
