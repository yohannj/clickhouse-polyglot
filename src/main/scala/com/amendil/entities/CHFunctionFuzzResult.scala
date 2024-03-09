package com.amendil.entities

import com.amendil.entities.CHFunctionIO._

/**
  * CHFunctionFuzzResult is used to contain intermediate and final result of the fuzzing algorithm.
  * We are keeping a distinction of all kind of functions, because that knowledge can be useful for fuzzing.
  *
  * @param name The name of the ClickHouse function being fuzzed.
  */
final case class CHFunctionFuzzResult(
    name: String,
    function0Ns: Seq[Function0N] = Nil,
    function1Ns: Seq[Function1N] = Nil,
    function2Ns: Seq[Function2N] = Nil,
    function3Ns: Seq[Function3N] = Nil,
    function0Opt: Option[Function0] = None,
    function1s: Seq[Function1] = Nil,
    function2s: Seq[Function2] = Nil,
    function3s: Seq[Function3] = Nil,
    function4s: Seq[Function4] = Nil,
    parametric1Function0Ns: Seq[Parametric1Function0N] = Nil,
    parametric1Function1Ns: Seq[Parametric1Function1N] = Nil,
    parametric1Function1s: Seq[Parametric1Function1] = Nil,
    parametric1Function2s: Seq[Parametric1Function2] = Nil,
    parametric2Function0Ns: Seq[Parametric2Function0N] = Nil,
    parametric2Function1Ns: Seq[Parametric2Function1N] = Nil,
    parametric2Function1s: Seq[Parametric2Function1] = Nil,
    parametric2Function2s: Seq[Parametric2Function2] = Nil,
    parametric3Function0Ns: Seq[Parametric3Function0N] = Nil,
    parametric3Function1Ns: Seq[Parametric3Function1N] = Nil,
    parametric3Function1s: Seq[Parametric3Function1] = Nil,
    parametric3Function2s: Seq[Parametric3Function2] = Nil
):
  // Helps working with all those functions by providing a single variable
  // And check inputs are unique
  val functions =
    function0Ns ++ function1Ns ++ function2Ns ++ function3Ns ++
      function0Opt.toSeq ++ function1s ++ function2s ++ function3s ++ function4s ++
      parametric1Function0Ns ++ parametric1Function1Ns ++ parametric1Function1s ++
      parametric1Function2s ++ parametric2Function0Ns ++ parametric2Function1Ns ++
      parametric2Function1s ++ parametric2Function2s ++ parametric3Function0Ns ++
      parametric3Function1Ns ++ parametric3Function1s ++ parametric3Function2s

  require(functions.groupBy(_.input).values.filter(_.size != 1).isEmpty)

  def atLeastOneSignatureFound(): Boolean = functions.nonEmpty
  def atLeastOneNonParametricSignatureFound(): Boolean =
    function0Ns.nonEmpty || function1Ns.nonEmpty || function2Ns.nonEmpty ||
      function3Ns.nonEmpty || function0Opt.nonEmpty || function1s.nonEmpty ||
      function2s.nonEmpty || function3s.nonEmpty || function4s.nonEmpty
