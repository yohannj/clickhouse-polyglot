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
    parametric0NFunction0Ns: Seq[Parametric0NFunction0N] = Nil,
    parametric0NFunction1Ns: Seq[Parametric0NFunction1N] = Nil,
    parametric0NFunction2Ns: Seq[Parametric0NFunction2N] = Nil,
    parametric0NFunction1s: Seq[Parametric0NFunction1] = Nil,
    parametric0NFunction2s: Seq[Parametric0NFunction2] = Nil,
    parametric0NFunction3s: Seq[Parametric0NFunction3] = Nil,
    parametric1NFunction0Ns: Seq[Parametric1NFunction0N] = Nil,
    parametric1NFunction1Ns: Seq[Parametric1NFunction1N] = Nil,
    parametric1NFunction2Ns: Seq[Parametric1NFunction2N] = Nil,
    parametric1NFunction1s: Seq[Parametric1NFunction1] = Nil,
    parametric1NFunction2s: Seq[Parametric1NFunction2] = Nil,
    parametric1NFunction3s: Seq[Parametric1NFunction3] = Nil,
    parametric2NFunction0Ns: Seq[Parametric2NFunction0N] = Nil,
    parametric2NFunction1Ns: Seq[Parametric2NFunction1N] = Nil,
    parametric2NFunction2Ns: Seq[Parametric2NFunction2N] = Nil,
    parametric2NFunction1s: Seq[Parametric2NFunction1] = Nil,
    parametric2NFunction2s: Seq[Parametric2NFunction2] = Nil,
    parametric2NFunction3s: Seq[Parametric2NFunction3] = Nil,
    parametric3NFunction0Ns: Seq[Parametric3NFunction0N] = Nil,
    parametric3NFunction1Ns: Seq[Parametric3NFunction1N] = Nil,
    parametric3NFunction2Ns: Seq[Parametric3NFunction2N] = Nil,
    parametric3NFunction1s: Seq[Parametric3NFunction1] = Nil,
    parametric3NFunction2s: Seq[Parametric3NFunction2] = Nil,
    parametric3NFunction3s: Seq[Parametric3NFunction3] = Nil,
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
    specialFunction0Ns: Seq[Function0N] = Nil,
    specialParametric2Function2Ns: Seq[Parametric2Function2N] = Nil,
    specialParametric2Function3Ns: Seq[Parametric2Function3N] = Nil
):
  // Helps working with all those functions by providing a single variable
  // And check inputs are unique
  val functions =
    function0Ns ++ function1Ns ++ function2Ns ++ function3Ns ++ function0Opt ++
      function1s ++ function2s ++ function3s ++ function4s ++ lambdaFunction0NOpt ++
      lambdaFunction1NOpt ++ parametric0NFunction0Ns ++ parametric0NFunction1Ns ++
      parametric0NFunction2Ns ++ parametric0NFunction1s ++ parametric0NFunction2s ++
      parametric0NFunction3s ++ parametric1NFunction0Ns ++ parametric1NFunction1Ns ++
      parametric1NFunction2Ns ++ parametric1NFunction1s ++ parametric1NFunction2s ++
      parametric1NFunction3s ++ parametric2NFunction0Ns ++ parametric2NFunction1Ns ++
      parametric2NFunction2Ns ++ parametric2NFunction1s ++ parametric2NFunction2s ++
      parametric2NFunction3s ++ parametric3NFunction0Ns ++ parametric3NFunction1Ns ++
      parametric3NFunction2Ns ++ parametric3NFunction1s ++ parametric3NFunction2s ++
      parametric3NFunction3s ++ parametric1Function0Ns ++ parametric1Function1Ns ++
      parametric1Function2Ns ++ parametric1Function1s ++ parametric1Function2s ++
      parametric1Function3s ++ parametric2Function0Ns ++ parametric2Function1Ns ++
      parametric2Function2Ns ++ parametric2Function1s ++ parametric2Function2s ++
      parametric2Function3s ++ parametric3Function0Ns ++ parametric3Function1Ns ++
      parametric3Function2Ns ++ parametric3Function1s ++ parametric3Function2s ++
      parametric3Function3s ++ parametric4Function0Ns ++ parametric4Function1Ns ++
      parametric4Function2Ns ++ parametric4Function1s ++ parametric4Function2s ++
      parametric4Function3s ++ specialFunction0Ns ++ specialParametric2Function2Ns ++
      specialParametric2Function3Ns

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
    parametric0NFunction0Ns.nonEmpty || parametric0NFunction1Ns.nonEmpty || parametric0NFunction2Ns.nonEmpty ||
      parametric0NFunction1s.nonEmpty || parametric0NFunction2s.nonEmpty || parametric0NFunction3s.nonEmpty ||
      parametric1NFunction0Ns.nonEmpty || parametric1NFunction1Ns.nonEmpty || parametric1NFunction2Ns.nonEmpty ||
      parametric1NFunction1s.nonEmpty || parametric1NFunction2s.nonEmpty || parametric1NFunction3s.nonEmpty ||
      parametric2NFunction0Ns.nonEmpty || parametric2NFunction1Ns.nonEmpty || parametric2NFunction2Ns.nonEmpty ||
      parametric2NFunction1s.nonEmpty || parametric2NFunction2s.nonEmpty || parametric2NFunction3s.nonEmpty ||
      parametric3NFunction0Ns.nonEmpty || parametric3NFunction1Ns.nonEmpty || parametric3NFunction2Ns.nonEmpty ||
      parametric3NFunction1s.nonEmpty || parametric3NFunction2s.nonEmpty || parametric3NFunction3s.nonEmpty ||
      parametric1Function0Ns.nonEmpty || parametric1Function1Ns.nonEmpty || parametric1Function2Ns.nonEmpty ||
      parametric1Function1s.nonEmpty || parametric1Function2s.nonEmpty || parametric1Function3s.nonEmpty ||
      parametric2Function0Ns.nonEmpty || parametric2Function1Ns.nonEmpty || parametric2Function2Ns.nonEmpty ||
      parametric2Function1s.nonEmpty || parametric2Function2s.nonEmpty || parametric2Function3s.nonEmpty ||
      parametric3Function0Ns.nonEmpty || parametric3Function1Ns.nonEmpty || parametric3Function2Ns.nonEmpty ||
      parametric3Function1s.nonEmpty || parametric3Function2s.nonEmpty || parametric3Function3s.nonEmpty ||
      parametric4Function0Ns.nonEmpty || parametric4Function1Ns.nonEmpty || parametric4Function2Ns.nonEmpty ||
      parametric4Function1s.nonEmpty || parametric4Function2s.nonEmpty || parametric4Function3s.nonEmpty
