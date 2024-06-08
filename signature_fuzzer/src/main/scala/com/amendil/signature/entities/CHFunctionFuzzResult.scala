package com.amendil.signature.entities

import com.amendil.signature.entities.CHFunctionIO.*
import com.typesafe.scalalogging.StrictLogging

/**
  * CHFunctionFuzzResult is used to contain intermediate and final result of the fuzzing algorithm.
  * We are keeping a distinction of all kind of functions, because that knowledge can be useful for fuzzing.
  *
  * @param name The name of the ClickHouse function being fuzzed.
  */
final case class CHFunctionFuzzResult(
    name: String,
    isAggregate: Boolean,
    aliasTo: String,
    modes: Set[CHFunction.Mode],
    function0Ns: Seq[Function0N],
    function0N1s: Seq[Function0N1],
    function1N1s: Seq[Function1N1],
    function1Ns: Seq[Function1N],
    function2Ns: Seq[Function2N],
    function3Ns: Seq[Function3N],
    function0Opt: Option[Function0],
    function1s: Seq[Function1],
    function2s: Seq[Function2],
    function3s: Seq[Function3],
    function4s: Seq[Function4],
    function5s: Seq[Function5],
    function6s: Seq[Function6],
    function7s: Seq[Function7],
    function8s: Seq[Function8],
    function9s: Seq[Function9],
    lambdaArrayFunction0NOpt: Option[LambdaArrayFunction0N],
    lambdaArrayFunction1NOpt: Option[LambdaArrayFunction1N],
    lambdaArrayFunction1N1Opt: Option[LambdaArrayFunction1N1],
    lambdaMapFunction1Opt: Option[LambdaMapFunction1],
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
    parametric0Function0Ns: Seq[Parametric0Function0N],
    parametric0Function1Ns: Seq[Parametric0Function1N],
    parametric0Function2Ns: Seq[Parametric0Function2N],
    parametric0Function1s: Seq[Parametric0Function1],
    parametric0Function2s: Seq[Parametric0Function2],
    parametric0Function3s: Seq[Parametric0Function3],
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

  val functions =
    // productIterator is an internal method in all "case class" to iterate over its constructor arguments
    productIterator.toSeq
      .collect {
        case s: Seq[?] if s.nonEmpty && s.head.isInstanceOf[CHFunctionIO] => s
        case Some(fn) if fn.isInstanceOf[CHFunctionIO]                    => Seq(fn)
      }
      .flatten
      .asInstanceOf[Seq[CHFunctionIO]]

  require(
    functions.groupBy(f => (f.parameters, f.arguments)).values.filter(_.size != 1).isEmpty,
    s"Function $name has multiple time the same signature"
  )
  logger.trace(s"CHFunctionFuzzResult - require validated")

  val atLeastOneSignatureFound: Boolean = functions.nonEmpty

  val isSpecialRepeatedFunction: Boolean =
    specialFunction0Ns.nonEmpty || specialParametric2Function2Ns.nonEmpty || specialParametric2Function3Ns.nonEmpty

  val isLambda: Boolean =
    lambdaArrayFunction0NOpt.nonEmpty || lambdaArrayFunction1NOpt.nonEmpty || lambdaArrayFunction1N1Opt.nonEmpty || lambdaMapFunction1Opt.nonEmpty

object CHFunctionFuzzResult:
  def apply(name: String, isAggregate: Boolean, aliasTo: String): CHFunctionFuzzResult =
    CHFunctionFuzzResult(
      name = name,
      isAggregate = isAggregate,
      aliasTo = aliasTo,
      modes = Set.empty,
      function0Ns = Nil,
      function0N1s = Nil,
      function1N1s = Nil,
      function1Ns = Nil,
      function2Ns = Nil,
      function3Ns = Nil,
      function0Opt = None,
      function1s = Nil,
      function2s = Nil,
      function3s = Nil,
      function4s = Nil,
      function5s = Nil,
      function6s = Nil,
      function7s = Nil,
      function8s = Nil,
      function9s = Nil,
      lambdaArrayFunction0NOpt = None,
      lambdaArrayFunction1NOpt = None,
      lambdaArrayFunction1N1Opt = None,
      lambdaMapFunction1Opt = None,
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
      parametric0Function0Ns = Nil,
      parametric0Function1Ns = Nil,
      parametric0Function2Ns = Nil,
      parametric0Function1s = Nil,
      parametric0Function2s = Nil,
      parametric0Function3s = Nil,
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
