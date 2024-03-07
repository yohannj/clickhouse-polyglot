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
    function4s: Seq[Function4] = Nil
):
  // Helps working with all those functions by providing a single variable
  // And check inputs are unique
  val functions =
    function0Ns ++ function1Ns ++ function2Ns ++ function3Ns ++
      function0Opt.toSeq ++ function1s ++ function2s ++ function3s ++ function4s
  require(functions.groupBy(_.input).values.filter(_.size != 1).isEmpty)

  def atLeastOneSignatureFound(): Boolean = functions.nonEmpty
  def atLeastOneFunctionNSignatureFound(): Boolean =
    function0Ns.nonEmpty || function1Ns.nonEmpty || function2Ns.nonEmpty
