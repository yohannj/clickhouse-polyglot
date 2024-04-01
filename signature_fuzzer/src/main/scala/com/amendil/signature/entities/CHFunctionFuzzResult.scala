package com.amendil.signature.entities

import com.amendil.signature.entities.CHFunctionIO._
import com.typesafe.scalalogging.StrictLogging

/**
  * CHFunctionFuzzResult is used to contain intermediate and final result of the fuzzing algorithm.
  * We are keeping a distinction of all kind of functions, because that knowledge can be useful for fuzzing.
  *
  * @param name The name of the ClickHouse function being fuzzed.
  */
final case class CHFunctionFuzzResult(
    name: String,
    modes: Set[CHFunction.Mode],
    function0Ns: Seq[Function0N],
    function1Ns: Seq[Function1N],
    function2Ns: Seq[Function2N],
    function3Ns: Seq[Function3N],
    function0Opt: Option[Function0],
    function1s: Seq[Function1],
    function2s: Seq[Function2],
    function3s: Seq[Function3],
    function4s: Seq[Function4],
    function5s: Seq[Function5],
    lambdaFunction0NOpt: Option[LambdaFunction0N],
    lambdaFunction1NOpt: Option[LambdaFunction1N],
    parametric0NFunction0Ns: Seq[Parametric0NFunction0N],
    parametric0NFunction1Ns: Seq[Parametric0NFunction1N],
    parametric0NFunction2Ns: Seq[Parametric0NFunction2N],
    parametric0NFunction1s: Seq[Parametric0NFunction1],
    parametric0NFunction2s: Seq[Parametric0NFunction2],
    parametric0NFunction3s: Seq[Parametric0NFunction3],
    parametric1NFunction0Ns: Seq[Parametric1NFunction0N],
    parametric1NFunction1Ns: Seq[Parametric1NFunction1N],
    parametric1NFunction2Ns: Seq[Parametric1NFunction2N],
    parametric1NFunction1s: Seq[Parametric1NFunction1],
    parametric1NFunction2s: Seq[Parametric1NFunction2],
    parametric1NFunction3s: Seq[Parametric1NFunction3],
    parametric2NFunction0Ns: Seq[Parametric2NFunction0N],
    parametric2NFunction1Ns: Seq[Parametric2NFunction1N],
    parametric2NFunction2Ns: Seq[Parametric2NFunction2N],
    parametric2NFunction1s: Seq[Parametric2NFunction1],
    parametric2NFunction2s: Seq[Parametric2NFunction2],
    parametric2NFunction3s: Seq[Parametric2NFunction3],
    parametric3NFunction0Ns: Seq[Parametric3NFunction0N],
    parametric3NFunction1Ns: Seq[Parametric3NFunction1N],
    parametric3NFunction2Ns: Seq[Parametric3NFunction2N],
    parametric3NFunction1s: Seq[Parametric3NFunction1],
    parametric3NFunction2s: Seq[Parametric3NFunction2],
    parametric3NFunction3s: Seq[Parametric3NFunction3],
    parametric1Function0Ns: Seq[Parametric1Function0N],
    parametric1Function1Ns: Seq[Parametric1Function1N],
    parametric1Function2Ns: Seq[Parametric1Function2N],
    parametric1Function1s: Seq[Parametric1Function1],
    parametric1Function2s: Seq[Parametric1Function2],
    parametric1Function3s: Seq[Parametric1Function3],
    parametric2Function0Ns: Seq[Parametric2Function0N],
    parametric2Function1Ns: Seq[Parametric2Function1N],
    parametric2Function2Ns: Seq[Parametric2Function2N],
    parametric2Function1s: Seq[Parametric2Function1],
    parametric2Function2s: Seq[Parametric2Function2],
    parametric2Function3s: Seq[Parametric2Function3],
    parametric3Function0Ns: Seq[Parametric3Function0N],
    parametric3Function1Ns: Seq[Parametric3Function1N],
    parametric3Function2Ns: Seq[Parametric3Function2N],
    parametric3Function1s: Seq[Parametric3Function1],
    parametric3Function2s: Seq[Parametric3Function2],
    parametric3Function3s: Seq[Parametric3Function3],
    parametric4Function0Ns: Seq[Parametric4Function0N],
    parametric4Function1Ns: Seq[Parametric4Function1N],
    parametric4Function2Ns: Seq[Parametric4Function2N],
    parametric4Function1s: Seq[Parametric4Function1],
    parametric4Function2s: Seq[Parametric4Function2],
    parametric4Function3s: Seq[Parametric4Function3],
    specialFunction0Ns: Seq[Function0N],
    specialParametric2Function2Ns: Seq[Parametric2Function2N],
    specialParametric2Function3Ns: Seq[Parametric2Function3N]
) extends StrictLogging:
  logger.trace(s"CHFunctionFuzzResult - init constructor")
  // Helps working with all those functions by providing a single variable
  // And check inputs are unique
  val functions =
    function0Ns ++ function1Ns ++ function2Ns ++ function3Ns ++ function0Opt ++
      function1s ++ function2s ++ function3s ++ function4s ++ function5s ++
      lambdaFunction0NOpt ++ lambdaFunction1NOpt ++ parametric0NFunction0Ns ++
      parametric0NFunction1Ns ++ parametric0NFunction2Ns ++ parametric0NFunction1s ++
      parametric0NFunction2s ++ parametric0NFunction3s ++ parametric1NFunction0Ns ++
      parametric1NFunction1Ns ++ parametric1NFunction2Ns ++ parametric1NFunction1s ++
      parametric1NFunction2s ++ parametric1NFunction3s ++ parametric2NFunction0Ns ++
      parametric2NFunction1Ns ++ parametric2NFunction2Ns ++ parametric2NFunction1s ++
      parametric2NFunction2s ++ parametric2NFunction3s ++ parametric3NFunction0Ns ++
      parametric3NFunction1Ns ++ parametric3NFunction2Ns ++ parametric3NFunction1s ++
      parametric3NFunction2s ++ parametric3NFunction3s ++ parametric1Function0Ns ++
      parametric1Function1Ns ++ parametric1Function2Ns ++ parametric1Function1s ++
      parametric1Function2s ++ parametric1Function3s ++ parametric2Function0Ns ++
      parametric2Function1Ns ++ parametric2Function2Ns ++ parametric2Function1s ++
      parametric2Function2s ++ parametric2Function3s ++ parametric3Function0Ns ++
      parametric3Function1Ns ++ parametric3Function2Ns ++ parametric3Function1s ++
      parametric3Function2s ++ parametric3Function3s ++ parametric4Function0Ns ++
      parametric4Function1Ns ++ parametric4Function2Ns ++ parametric4Function1s ++
      parametric4Function2s ++ parametric4Function3s ++ specialFunction0Ns ++
      specialParametric2Function2Ns ++ specialParametric2Function3Ns

  require(
    functions.groupBy(f => (f.parameters, f.arguments)).values.filter(_.size != 1).isEmpty,
    s"Function $name has multiple time the same signature"
  )

  logger.trace(s"CHFunctionFuzzResult - require validated")

  val atLeastOneSignatureFound: Boolean = functions.nonEmpty

  val isSpecialInfiniteFunction: Boolean =
    specialFunction0Ns.nonEmpty || specialParametric2Function2Ns.nonEmpty || specialParametric2Function3Ns.nonEmpty

  val isLambda: Boolean = lambdaFunction0NOpt.nonEmpty || lambdaFunction1NOpt.nonEmpty

object CHFunctionFuzzResult {
  def apply(name: String): CHFunctionFuzzResult =
    CHFunctionFuzzResult(
      name = name,
      modes = Set.empty,
      function0Ns = Nil,
      function1Ns = Nil,
      function2Ns = Nil,
      function3Ns = Nil,
      function0Opt = None,
      function1s = Nil,
      function2s = Nil,
      function3s = Nil,
      function4s = Nil,
      function5s = Nil,
      lambdaFunction0NOpt = None,
      lambdaFunction1NOpt = None,
      parametric0NFunction0Ns = Nil,
      parametric0NFunction1Ns = Nil,
      parametric0NFunction2Ns = Nil,
      parametric0NFunction1s = Nil,
      parametric0NFunction2s = Nil,
      parametric0NFunction3s = Nil,
      parametric1NFunction0Ns = Nil,
      parametric1NFunction1Ns = Nil,
      parametric1NFunction2Ns = Nil,
      parametric1NFunction1s = Nil,
      parametric1NFunction2s = Nil,
      parametric1NFunction3s = Nil,
      parametric2NFunction0Ns = Nil,
      parametric2NFunction1Ns = Nil,
      parametric2NFunction2Ns = Nil,
      parametric2NFunction1s = Nil,
      parametric2NFunction2s = Nil,
      parametric2NFunction3s = Nil,
      parametric3NFunction0Ns = Nil,
      parametric3NFunction1Ns = Nil,
      parametric3NFunction2Ns = Nil,
      parametric3NFunction1s = Nil,
      parametric3NFunction2s = Nil,
      parametric3NFunction3s = Nil,
      parametric1Function0Ns = Nil,
      parametric1Function1Ns = Nil,
      parametric1Function2Ns = Nil,
      parametric1Function1s = Nil,
      parametric1Function2s = Nil,
      parametric1Function3s = Nil,
      parametric2Function0Ns = Nil,
      parametric2Function1Ns = Nil,
      parametric2Function2Ns = Nil,
      parametric2Function1s = Nil,
      parametric2Function2s = Nil,
      parametric2Function3s = Nil,
      parametric3Function0Ns = Nil,
      parametric3Function1Ns = Nil,
      parametric3Function2Ns = Nil,
      parametric3Function1s = Nil,
      parametric3Function2s = Nil,
      parametric3Function3s = Nil,
      parametric4Function0Ns = Nil,
      parametric4Function1Ns = Nil,
      parametric4Function2Ns = Nil,
      parametric4Function1s = Nil,
      parametric4Function2s = Nil,
      parametric4Function3s = Nil,
      specialFunction0Ns = Nil,
      specialParametric2Function2Ns = Nil,
      specialParametric2Function3Ns = Nil
    )
}
