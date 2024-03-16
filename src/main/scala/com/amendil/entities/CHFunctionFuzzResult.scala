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
    lambdaFunction0NOpt: Option[LambdaFunction0N] = None,
    lambdaFunction1NOpt: Option[LambdaFunction1N] = None,
    parametric1Function0Ns: Seq[Parametric1Function0N] = Nil,
    parametric1Function1Ns: Seq[Parametric1Function1N] = Nil,
    parametric1Function2Ns: Seq[Parametric1Function2N] = Nil,
    parametric1Function1s: Seq[Parametric1Function1] = Nil,
    parametric1Function2s: Seq[Parametric1Function2] = Nil,
    parametric1Function3s: Seq[Parametric1Function3] = Nil,
    parametric2Function0Ns: Seq[Parametric2Function0N] = Nil,
    parametric2Function1Ns: Seq[Parametric2Function1N] = Nil,
    parametric2Function2Ns: Seq[Parametric2Function2N] = Nil,
    parametric2Function1s: Seq[Parametric2Function1] = Nil,
    parametric2Function2s: Seq[Parametric2Function2] = Nil,
    parametric2Function3s: Seq[Parametric2Function3] = Nil,
    parametric3Function0Ns: Seq[Parametric3Function0N] = Nil,
    parametric3Function1Ns: Seq[Parametric3Function1N] = Nil,
    parametric3Function2Ns: Seq[Parametric3Function2N] = Nil,
    parametric3Function1s: Seq[Parametric3Function1] = Nil,
    parametric3Function2s: Seq[Parametric3Function2] = Nil,
    parametric3Function3s: Seq[Parametric3Function3] = Nil,
    parametric4Function0Ns: Seq[Parametric4Function0N] = Nil,
    parametric4Function1Ns: Seq[Parametric4Function1N] = Nil,
    parametric4Function2Ns: Seq[Parametric4Function2N] = Nil,
    parametric4Function1s: Seq[Parametric4Function1] = Nil,
    parametric4Function2s: Seq[Parametric4Function2] = Nil,
    parametric4Function3s: Seq[Parametric4Function3] = Nil,
    parametric5Function0Ns: Seq[Parametric5Function0N] = Nil,
    parametric5Function1Ns: Seq[Parametric5Function1N] = Nil,
    parametric5Function2Ns: Seq[Parametric5Function2N] = Nil,
    parametric5Function1s: Seq[Parametric5Function1] = Nil,
    parametric5Function2s: Seq[Parametric5Function2] = Nil,
    parametric5Function3s: Seq[Parametric5Function3] = Nil,
    specialFunction0Ns: Seq[Function0N] = Nil,
    specialParametric2Function2Ns: Seq[Parametric2Function2N] = Nil,
    specialParametric2Function3Ns: Seq[Parametric2Function3N] = Nil
):
  // Helps working with all those functions by providing a single variable
  // And check inputs are unique
  val functions =
    function0Ns ++ function1Ns ++ function2Ns ++ function3Ns ++
      function0Opt.toSeq ++ function1s ++ function2s ++ function3s ++ function4s ++
      lambdaFunction0NOpt.toSeq ++ lambdaFunction1NOpt.toSeq ++ parametric1Function0Ns ++
      parametric1Function1Ns ++ parametric1Function2Ns ++ parametric1Function1s ++
      parametric1Function2s ++ parametric1Function3s ++ parametric2Function0Ns ++
      parametric2Function1Ns ++ parametric2Function2Ns ++ parametric2Function1s ++
      parametric2Function2s ++ parametric2Function3s ++ parametric3Function0Ns ++
      parametric3Function1Ns ++ parametric3Function2Ns ++ parametric3Function1s ++
      parametric3Function2s ++ parametric3Function3s ++ parametric4Function0Ns ++
      parametric4Function1Ns ++ parametric4Function2Ns ++ parametric4Function1s ++
      parametric4Function2s ++ parametric4Function3s ++ parametric5Function0Ns ++
      parametric5Function1Ns ++ parametric5Function2Ns ++ parametric5Function1s ++
      parametric5Function2s ++ parametric5Function3s ++ specialFunction0Ns ++
      specialParametric2Function2Ns ++ specialParametric2Function3Ns

  require(
    functions.groupBy(f => (f.parameters, f.arguments)).values.filter(_.size != 1).isEmpty,
    s"Function $name has multiple time the same signature"
  )

  val atLeastOneSignatureFound: Boolean = functions.nonEmpty

  val isSpecialInfiniteFunction: Boolean =
    specialFunction0Ns.nonEmpty || specialParametric2Function2Ns.nonEmpty || specialParametric2Function3Ns.nonEmpty

  val isLambda: Boolean = lambdaFunction0NOpt.nonEmpty || lambdaFunction1NOpt.nonEmpty

  val isNonParametric: Boolean =
    function0Ns.nonEmpty || function1Ns.nonEmpty || function2Ns.nonEmpty ||
      function3Ns.nonEmpty || function0Opt.nonEmpty || function1s.nonEmpty ||
      function2s.nonEmpty || function3s.nonEmpty || function4s.nonEmpty

  val isParametric: Boolean =
    parametric1Function0Ns.nonEmpty || parametric1Function1Ns.nonEmpty || parametric1Function2Ns.nonEmpty ||
      parametric1Function1s.nonEmpty || parametric1Function2s.nonEmpty || parametric1Function3s.nonEmpty ||
      parametric2Function0Ns.nonEmpty || parametric2Function1Ns.nonEmpty || parametric2Function2Ns.nonEmpty ||
      parametric2Function1s.nonEmpty || parametric2Function2s.nonEmpty || parametric2Function3s.nonEmpty ||
      parametric3Function0Ns.nonEmpty || parametric3Function1Ns.nonEmpty || parametric3Function2Ns.nonEmpty ||
      parametric3Function1s.nonEmpty || parametric3Function2s.nonEmpty || parametric3Function3s.nonEmpty ||
      parametric4Function0Ns.nonEmpty || parametric4Function1Ns.nonEmpty || parametric4Function2Ns.nonEmpty ||
      parametric4Function1s.nonEmpty || parametric4Function2s.nonEmpty || parametric4Function3s.nonEmpty ||
      parametric5Function0Ns.nonEmpty || parametric5Function1Ns.nonEmpty || parametric5Function2Ns.nonEmpty ||
      parametric5Function1s.nonEmpty || parametric5Function2s.nonEmpty || parametric5Function3s.nonEmpty
