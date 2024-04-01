package com.amendil.signature.fuzz

import com.amendil.common.ConcurrencyUtils._
import com.amendil.common.entities._
import com.amendil.common.http.CHClient
import com.amendil.signature.Settings
import com.amendil.signature.entities._
import com.amendil.signature.entities.CHFunctionIO._
import com.amendil.signature.fuzz.Fuzzer._
import com.typesafe.scalalogging.StrictLogging

import scala.annotation.targetName
import scala.concurrent.{ExecutionContext, Future}

object FuzzerParametricFunctions extends StrictLogging:

  // Remove some types that are obviously not parameters
  private val parametricAbstractType: Seq[CHFuzzableAbstractType] = CHFuzzableAbstractType.values.toSeq.filterNot {
    abstractType =>
      abstractType.fuzzingValues.isEmpty ||
      abstractType.fuzzingValues.head.contains("::Array(") ||
      abstractType.fuzzingValues.head.contains("array(") ||
      abstractType.fuzzingValues.head.contains("::Map(") ||
      abstractType.fuzzingValues.head.contains("map(") ||
      abstractType.fuzzingValues.head.contains("::Tuple(") ||
      abstractType.fuzzingValues.head.contains("::Date") ||
      abstractType.fuzzingValues.head.contains("::IPv4") ||
      abstractType.fuzzingValues.head.contains("::IPv6") ||
      abstractType.fuzzingValues.head.contains("::JSON") ||
      abstractType.fuzzingValues.head.contains("::UUID") ||
      abstractType.fuzzingValues.head.contains("::Enum") ||
      abstractType.fuzzingValues.head.contains("::Point") ||
      abstractType.fuzzingValues.head.contains("::Ring") ||
      abstractType.fuzzingValues.head.contains("::Polygon") ||
      abstractType.fuzzingValues.head.contains("::MultiPolygon") ||
      abstractType.fuzzingValues.head.contains("::Interval")
  }

  private[fuzz] def fuzzingFunctionWithCost(
      using client: CHClient,
      ec: ExecutionContext
  ): Seq[((CHFunctionFuzzResult) => Future[CHFunctionFuzzResult], Long)] =
    val paramCount = parametricAbstractType.flatMap(_.chFuzzableTypes).size.toLong
    val argCount = CHFuzzableType.values.size.toLong
    Seq(
      (fuzzFunction1Or0NWith1Or0NParameter, paramCount * argCount),
      (fuzzFunction2Or1NWith1Or0NParameter, paramCount * argCount * argCount),
      // XXX (fuzzFunction3Or2NWith1Or0NParameter, paramCount * argCount * argCount * argCount),
      // Functions below MUST happen after the "With1Or0NParameter", they are then very easy to compute
      (fuzzFunction1With2Or1NParameters, paramCount * argCount + 1),
      (fuzzFunction0NWith2Or1NParameters, paramCount * argCount + 1),
      (fuzzFunction2With2Or1NParameters, paramCount * argCount * argCount + 1),
      (fuzzFunction1NWith2Or1NParameters, paramCount * argCount * argCount + 1),
      (fuzzFunction3With2Or1NParameters, paramCount * argCount * argCount * argCount + 1),
      (fuzzFunction2NWith2Or1NParameters, paramCount * argCount * argCount * argCount + 1),
      // Functions below MUST happen after the "With2Or1NParameter", they are then very easy to compute
      (fuzzFunction1With3Parameters, paramCount * argCount + 2),
      (fuzzFunction0NWith3Parameters, paramCount * argCount + 2),
      (fuzzFunction2With3Parameters, paramCount * argCount * argCount + 2),
      (fuzzFunction1NWith3Parameters, paramCount * argCount * argCount + 2),
      (fuzzFunction3With3Parameters, paramCount * argCount * argCount * argCount + 2),
      (fuzzFunction2NWith3Parameters, paramCount * argCount * argCount * argCount + 2)
      // Functions below MUST happen after the "With3Parameter", they are then very easy to compute
      // (fuzzFunction1With4Parameters, paramCount * argCount + 3),
      // (fuzzFunction0NWith4Parameters, paramCount * argCount + 3),
      // (fuzzFunction2With4Parameters, paramCount * argCount * argCount + 3),
      // (fuzzFunction1NWith4Parameters, paramCount * argCount * argCount + 3),
      // (fuzzFunction3With4Parameters, paramCount * argCount * argCount * argCount + 3),
      // (fuzzFunction2NWith4Parameters, paramCount * argCount * argCount * argCount + 3),
    )

  private def fuzzFunction1Or0NWith1Or0NParameter(
      fn: CHFunctionFuzzResult
  )(using client: CHClient, ec: ExecutionContext): Future[CHFunctionFuzzResult] =
    logger.debug("fuzzFunction1Or0NWith1Or0NParameter")
    for
      functions: Seq[(ParametricFunctionInput, OutputType)] <-
        fuzzParametricFunction(fn.name, paramCount = 1, argCount = 1)
      fnHasInfiniteParams: Boolean <-
        if functions.isEmpty then Future.successful(true) else testInfiniteParamsFunctions(fn.name, functions.head._1)
      fnHasInfiniteArgs: Boolean <-
        if functions.isEmpty then Future.successful(true) else testInfiniteArgsFunctions(fn.name, functions.head._1)
    yield {
      if fnHasInfiniteParams && fnHasInfiniteArgs then
        fn.copy(parametric0NFunction0Ns = functions.map(toFn(_, CHFunctionIO.Parametric0NFunction0N.apply)))
      else if fnHasInfiniteParams && !fnHasInfiniteArgs then
        fn.copy(parametric0NFunction1s = functions.map(toFn(_, CHFunctionIO.Parametric0NFunction1.apply)))
      else if !fnHasInfiniteParams && fnHasInfiniteArgs then
        fn.copy(parametric1Function0Ns = functions.map(toFn(_, CHFunctionIO.Parametric1Function0N.apply)))
      else fn.copy(parametric1Function1s = functions.map(toFn(_, CHFunctionIO.Parametric1Function1.apply)))
    }

  private def fuzzFunction2Or1NWith1Or0NParameter(
      fn: CHFunctionFuzzResult
  )(using client: CHClient, ec: ExecutionContext): Future[CHFunctionFuzzResult] =
    logger.debug("fuzzFunction2Or1NWith1Or0NParameter")
    if fn.parametric0NFunction0Ns.nonEmpty || fn.parametric1Function0Ns.nonEmpty
    then Future.successful(fn)
    else
      for
        functions: Seq[(ParametricFunctionInput, OutputType)] <-
          fuzzParametricFunction(fn.name, paramCount = 1, argCount = 2)
        fnHasInfiniteParams: Boolean <-
          if functions.isEmpty then Future.successful(true) else testInfiniteParamsFunctions(fn.name, functions.head._1)
        fnHasInfiniteArgs: Boolean <-
          if functions.isEmpty then Future.successful(true) else testInfiniteArgsFunctions(fn.name, functions.head._1)
      yield {
        if fnHasInfiniteParams && fnHasInfiniteArgs then
          fn.copy(parametric0NFunction1Ns = functions.map(toFn(_, CHFunctionIO.Parametric0NFunction1N.apply)))
        else if fnHasInfiniteParams && !fnHasInfiniteArgs then
          fn.copy(parametric0NFunction2s = functions.map(toFn(_, CHFunctionIO.Parametric0NFunction2.apply)))
        else if !fnHasInfiniteParams && fnHasInfiniteArgs then
          fn.copy(parametric1Function1Ns = functions.map(toFn(_, CHFunctionIO.Parametric1Function1N.apply)))
        else fn.copy(parametric1Function2s = functions.map(toFn(_, CHFunctionIO.Parametric1Function2.apply)))
      }

  private def fuzzFunction3Or2NWith1Or0NParameter(
      fn: CHFunctionFuzzResult
  )(using client: CHClient, ec: ExecutionContext): Future[CHFunctionFuzzResult] =
    logger.debug("fuzzFunction3Or2NWith1Or0NParameter")
    if fn.parametric0NFunction0Ns.nonEmpty || fn.parametric1Function0Ns.nonEmpty ||
      fn.parametric0NFunction1Ns.nonEmpty || fn.parametric1Function1Ns.nonEmpty || (
        (fn.parametric0NFunction1s.filterNot(_.arg1.name.startsWith("Tuple")).nonEmpty ||
          fn.parametric1Function1s
            .filterNot(_.arg1.name.startsWith("Tuple"))
            .nonEmpty) && (fn.parametric0NFunction2s.isEmpty && fn.parametric1Function2s.isEmpty)
      )
    then Future.successful(fn)
    else
      for
        functions: Seq[(ParametricFunctionInput, OutputType)] <-
          fuzzParametricFunction(fn.name, paramCount = 1, argCount = 3)
        fnHasInfiniteParams: Boolean <-
          if functions.isEmpty then Future.successful(true) else testInfiniteParamsFunctions(fn.name, functions.head._1)
        fnHasInfiniteArgs: Boolean <-
          if functions.isEmpty then Future.successful(true) else testInfiniteArgsFunctions(fn.name, functions.head._1)
      yield {
        if fnHasInfiniteParams && fnHasInfiniteArgs then
          fn.copy(parametric0NFunction2Ns = functions.map(toFn(_, CHFunctionIO.Parametric0NFunction2N.apply)))
        else if fnHasInfiniteParams && !fnHasInfiniteArgs then
          fn.copy(parametric0NFunction3s = functions.map(toFn(_, CHFunctionIO.Parametric0NFunction3.apply)))
        else if !fnHasInfiniteParams && fnHasInfiniteArgs then
          fn.copy(parametric1Function2Ns = functions.map(toFn(_, CHFunctionIO.Parametric1Function2N.apply)))
        else fn.copy(parametric1Function3s = functions.map(toFn(_, CHFunctionIO.Parametric1Function3.apply)))
      }

  private def fuzzFunction1With2Or1NParameters(
      fn: CHFunctionFuzzResult
  )(using client: CHClient, ec: ExecutionContext): Future[CHFunctionFuzzResult] =
    logger.debug("fuzzFunction1With2Or1NParameters")
    if fn.parametric1Function1s.isEmpty then Future.successful(fn)
    else
      fuzzAddOneParameter(
        fn.name,
        fn.parametric1Function1s,
        (f: Parametric1Function1, param: CHFuzzableType) =>
          CHFunctionIO.Parametric2Function1(f.paramArg1, param, f.arg1, f.output),
        (f: Parametric1Function1, param: CHFuzzableType) =>
          CHFunctionIO.Parametric1NFunction1(f.paramArg1, param, f.arg1, f.output),
        (finiteFunctions: Seq[Parametric2Function1]) => fn.copy(parametric2Function1s = finiteFunctions),
        (infiniteFunctions: Seq[Parametric1NFunction1]) => fn.copy(parametric1NFunction1s = infiniteFunctions)
      )

  private def fuzzFunction1With3Parameters(
      fn: CHFunctionFuzzResult
  )(using client: CHClient, ec: ExecutionContext): Future[CHFunctionFuzzResult] =
    logger.debug("fuzzFunction1With3Parameters")
    if fn.parametric2Function1s.isEmpty then Future.successful(fn)
    else
      fuzzAddOneParameter(
        fn.name,
        fn.parametric2Function1s,
        (f: Parametric2Function1, param: CHFuzzableType) =>
          CHFunctionIO.Parametric3Function1(f.paramArg1, f.paramArg2, param, f.arg1, f.output),
        (f: Parametric2Function1, param: CHFuzzableType) =>
          CHFunctionIO.Parametric2NFunction1(f.paramArg1, f.paramArg2, param, f.arg1, f.output),
        (finiteFunctions: Seq[Parametric3Function1]) => fn.copy(parametric3Function1s = finiteFunctions),
        (infiniteFunctions: Seq[Parametric2NFunction1]) => fn.copy(parametric2NFunction1s = infiniteFunctions)
      )

  private def fuzzFunction1With4Parameters(
      fn: CHFunctionFuzzResult
  )(using client: CHClient, ec: ExecutionContext): Future[CHFunctionFuzzResult] =
    logger.debug("fuzzFunction1With4Parameters")
    if fn.parametric3Function1s.isEmpty then Future.successful(fn)
    else
      fuzzAddOneParameter(
        fn.name,
        fn.parametric3Function1s,
        (f: Parametric3Function1, param: CHFuzzableType) =>
          CHFunctionIO.Parametric4Function1(f.paramArg1, f.paramArg2, f.paramArg3, param, f.arg1, f.output),
        (f: Parametric3Function1, param: CHFuzzableType) =>
          CHFunctionIO.Parametric3NFunction1(f.paramArg1, f.paramArg2, f.paramArg3, param, f.arg1, f.output),
        (finiteFunctions: Seq[Parametric4Function1]) => fn.copy(parametric4Function1s = finiteFunctions),
        (infiniteFunctions: Seq[Parametric3NFunction1]) => fn.copy(parametric3NFunction1s = infiniteFunctions)
      )

  private def fuzzFunction2With2Or1NParameters(
      fn: CHFunctionFuzzResult
  )(using client: CHClient, ec: ExecutionContext): Future[CHFunctionFuzzResult] =
    logger.debug("fuzzFunction2With2Or1NParameters")
    if fn.parametric1Function2s.isEmpty then Future.successful(fn)
    else
      fuzzAddOneParameter(
        fn.name,
        fn.parametric1Function2s,
        (f: Parametric1Function2, param: CHFuzzableType) =>
          CHFunctionIO.Parametric2Function2(f.paramArg1, param, f.arg1, f.arg2, f.output),
        (f: Parametric1Function2, param: CHFuzzableType) =>
          CHFunctionIO.Parametric1NFunction2(f.paramArg1, param, f.arg1, f.arg2, f.output),
        (finiteFunctions: Seq[Parametric2Function2]) => fn.copy(parametric2Function2s = finiteFunctions),
        (infiniteFunctions: Seq[Parametric1NFunction2]) => fn.copy(parametric1NFunction2s = infiniteFunctions)
      )

  private def fuzzFunction2With3Parameters(
      fn: CHFunctionFuzzResult
  )(using client: CHClient, ec: ExecutionContext): Future[CHFunctionFuzzResult] =
    logger.debug("fuzzFunction2With3Parameters")
    if fn.parametric2Function2s.isEmpty then Future.successful(fn)
    else
      fuzzAddOneParameter(
        fn.name,
        fn.parametric2Function2s,
        (f: Parametric2Function2, param: CHFuzzableType) =>
          CHFunctionIO.Parametric3Function2(f.paramArg1, f.paramArg2, param, f.arg1, f.arg2, f.output),
        (f: Parametric2Function2, param: CHFuzzableType) =>
          CHFunctionIO.Parametric2NFunction2(f.paramArg1, f.paramArg2, param, f.arg1, f.arg2, f.output),
        (finiteFunctions: Seq[Parametric3Function2]) => fn.copy(parametric3Function2s = finiteFunctions),
        (infiniteFunctions: Seq[Parametric2NFunction2]) => fn.copy(parametric2NFunction2s = infiniteFunctions)
      )

  private def fuzzFunction2With4Parameters(
      fn: CHFunctionFuzzResult
  )(using client: CHClient, ec: ExecutionContext): Future[CHFunctionFuzzResult] =
    logger.debug("fuzzFunction2With4Parameters")
    if fn.parametric3Function2s.isEmpty then Future.successful(fn)
    else
      fuzzAddOneParameter(
        fn.name,
        fn.parametric3Function2s,
        (f: Parametric3Function2, param: CHFuzzableType) =>
          CHFunctionIO.Parametric4Function2(f.paramArg1, f.paramArg2, f.paramArg3, param, f.arg1, f.arg2, f.output),
        (f: Parametric3Function2, param: CHFuzzableType) =>
          CHFunctionIO.Parametric3NFunction2(f.paramArg1, f.paramArg2, f.paramArg3, param, f.arg1, f.arg2, f.output),
        (finiteFunctions: Seq[Parametric4Function2]) => fn.copy(parametric4Function2s = finiteFunctions),
        (infiniteFunctions: Seq[Parametric3NFunction2]) => fn.copy(parametric3NFunction2s = infiniteFunctions)
      )

  private def fuzzFunction3With2Or1NParameters(
      fn: CHFunctionFuzzResult
  )(using client: CHClient, ec: ExecutionContext): Future[CHFunctionFuzzResult] =
    logger.debug("fuzzFunction3With2Or1NParameters")
    if fn.parametric1Function3s.isEmpty then Future.successful(fn)
    else
      fuzzAddOneParameter(
        fn.name,
        fn.parametric1Function3s,
        (f: Parametric1Function3, param: CHFuzzableType) =>
          CHFunctionIO.Parametric2Function3(f.paramArg1, param, f.arg1, f.arg2, f.arg3, f.output),
        (f: Parametric1Function3, param: CHFuzzableType) =>
          CHFunctionIO.Parametric1NFunction3(f.paramArg1, param, f.arg1, f.arg2, f.arg3, f.output),
        (finiteFunctions: Seq[Parametric2Function3]) => fn.copy(parametric2Function3s = finiteFunctions),
        (infiniteFunctions: Seq[Parametric1NFunction3]) => fn.copy(parametric1NFunction3s = infiniteFunctions)
      )

  private def fuzzFunction3With3Parameters(
      fn: CHFunctionFuzzResult
  )(using client: CHClient, ec: ExecutionContext): Future[CHFunctionFuzzResult] =
    logger.debug("fuzzFunction3With3Parameters")
    if fn.parametric2Function3s.isEmpty then Future.successful(fn)
    else
      fuzzAddOneParameter(
        fn.name,
        fn.parametric2Function3s,
        (f: Parametric2Function3, param: CHFuzzableType) =>
          CHFunctionIO.Parametric3Function3(f.paramArg1, f.paramArg2, param, f.arg1, f.arg2, f.arg3, f.output),
        (f: Parametric2Function3, param: CHFuzzableType) =>
          CHFunctionIO.Parametric2NFunction3(f.paramArg1, f.paramArg2, param, f.arg1, f.arg2, f.arg3, f.output),
        (finiteFunctions: Seq[Parametric3Function3]) => fn.copy(parametric3Function3s = finiteFunctions),
        (infiniteFunctions: Seq[Parametric2NFunction3]) => fn.copy(parametric2NFunction3s = infiniteFunctions)
      )

  private def fuzzFunction3With4Parameters(
      fn: CHFunctionFuzzResult
  )(using client: CHClient, ec: ExecutionContext): Future[CHFunctionFuzzResult] =
    logger.debug("fuzzFunction3With4Parameters")
    if fn.parametric3Function3s.isEmpty then Future.successful(fn)
    else
      fuzzAddOneParameter(
        fn.name,
        fn.parametric3Function3s,
        (f: Parametric3Function3, param: CHFuzzableType) =>
          CHFunctionIO.Parametric4Function3(
            f.paramArg1,
            f.paramArg2,
            f.paramArg3,
            param,
            f.arg1,
            f.arg2,
            f.arg3,
            f.output
          ),
        (f: Parametric3Function3, param: CHFuzzableType) =>
          CHFunctionIO.Parametric3NFunction3(
            f.paramArg1,
            f.paramArg2,
            f.paramArg3,
            param,
            f.arg1,
            f.arg2,
            f.arg3,
            f.output
          ),
        (finiteFunctions: Seq[Parametric4Function3]) => fn.copy(parametric4Function3s = finiteFunctions),
        (infiniteFunctions: Seq[Parametric3NFunction3]) => fn.copy(parametric3NFunction3s = infiniteFunctions)
      )

  private def fuzzFunction0NWith2Or1NParameters(
      fn: CHFunctionFuzzResult
  )(using client: CHClient, ec: ExecutionContext): Future[CHFunctionFuzzResult] =
    logger.debug("fuzzFunction0NWith2Or1NParameters")
    if fn.parametric1Function0Ns.isEmpty then Future.successful(fn)
    else
      fuzzAddOneParameter(
        fn.name,
        fn.parametric1Function0Ns,
        (f: Parametric1Function0N, param: CHFuzzableType) =>
          CHFunctionIO.Parametric2Function0N(f.paramArg1, param, f.argN, f.output),
        (f: Parametric1Function0N, param: CHFuzzableType) =>
          CHFunctionIO.Parametric1NFunction0N(f.paramArg1, param, f.argN, f.output),
        (finiteFunctions: Seq[Parametric2Function0N]) => fn.copy(parametric2Function0Ns = finiteFunctions),
        (infiniteFunctions: Seq[Parametric1NFunction0N]) => fn.copy(parametric1NFunction0Ns = infiniteFunctions)
      )

  private def fuzzFunction0NWith3Parameters(
      fn: CHFunctionFuzzResult
  )(using client: CHClient, ec: ExecutionContext): Future[CHFunctionFuzzResult] =
    logger.debug("fuzzFunction0NWith3Parameters")
    if fn.parametric2Function0Ns.isEmpty then Future.successful(fn)
    else
      fuzzAddOneParameter(
        fn.name,
        fn.parametric2Function0Ns,
        (f: Parametric2Function0N, param: CHFuzzableType) =>
          CHFunctionIO.Parametric3Function0N(f.paramArg1, f.paramArg2, param, f.argN, f.output),
        (f: Parametric2Function0N, param: CHFuzzableType) =>
          CHFunctionIO.Parametric2NFunction0N(f.paramArg1, f.paramArg2, param, f.argN, f.output),
        (finiteFunctions: Seq[Parametric3Function0N]) => fn.copy(parametric3Function0Ns = finiteFunctions),
        (infiniteFunctions: Seq[Parametric2NFunction0N]) => fn.copy(parametric2NFunction0Ns = infiniteFunctions)
      )

  private def fuzzFunction0NWith4Parameters(
      fn: CHFunctionFuzzResult
  )(using client: CHClient, ec: ExecutionContext): Future[CHFunctionFuzzResult] =
    logger.debug("fuzzFunction0NWith4Parameters")
    if fn.parametric3Function0Ns.isEmpty then Future.successful(fn)
    else
      fuzzAddOneParameter(
        fn.name,
        fn.parametric3Function0Ns,
        (f: Parametric3Function0N, param: CHFuzzableType) =>
          CHFunctionIO.Parametric4Function0N(f.paramArg1, f.paramArg2, f.paramArg3, param, f.argN, f.output),
        (f: Parametric3Function0N, param: CHFuzzableType) =>
          CHFunctionIO.Parametric3NFunction0N(f.paramArg1, f.paramArg2, f.paramArg3, param, f.argN, f.output),
        (finiteFunctions: Seq[Parametric4Function0N]) => fn.copy(parametric4Function0Ns = finiteFunctions),
        (infiniteFunctions: Seq[Parametric3NFunction0N]) => fn.copy(parametric3NFunction0Ns = infiniteFunctions)
      )

  private def fuzzFunction1NWith2Or1NParameters(
      fn: CHFunctionFuzzResult
  )(using client: CHClient, ec: ExecutionContext): Future[CHFunctionFuzzResult] =
    logger.debug("fuzzFunction1NWith2Or1NParameters")
    if fn.parametric1Function1Ns.isEmpty then Future.successful(fn)
    else
      fuzzAddOneParameter(
        fn.name,
        fn.parametric1Function1Ns,
        (f: Parametric1Function1N, param: CHFuzzableType) =>
          CHFunctionIO.Parametric2Function1N(f.paramArg1, param, f.arg1, f.argN, f.output),
        (f: Parametric1Function1N, param: CHFuzzableType) =>
          CHFunctionIO.Parametric1NFunction1N(f.paramArg1, param, f.arg1, f.argN, f.output),
        (finiteFunctions: Seq[Parametric2Function1N]) => fn.copy(parametric2Function1Ns = finiteFunctions),
        (infiniteFunctions: Seq[Parametric1NFunction1N]) => fn.copy(parametric1NFunction1Ns = infiniteFunctions)
      )

  private def fuzzFunction1NWith3Parameters(
      fn: CHFunctionFuzzResult
  )(using client: CHClient, ec: ExecutionContext): Future[CHFunctionFuzzResult] =
    logger.debug("fuzzFunction1NWith3Parameters")
    if fn.parametric2Function1Ns.isEmpty then Future.successful(fn)
    else
      fuzzAddOneParameter(
        fn.name,
        fn.parametric2Function1Ns,
        (f: Parametric2Function1N, param: CHFuzzableType) =>
          CHFunctionIO.Parametric3Function1N(f.paramArg1, f.paramArg2, param, f.arg1, f.argN, f.output),
        (f: Parametric2Function1N, param: CHFuzzableType) =>
          CHFunctionIO.Parametric2NFunction1N(f.paramArg1, f.paramArg2, param, f.arg1, f.argN, f.output),
        (finiteFunctions: Seq[Parametric3Function1N]) => fn.copy(parametric3Function1Ns = finiteFunctions),
        (infiniteFunctions: Seq[Parametric2NFunction1N]) => fn.copy(parametric2NFunction1Ns = infiniteFunctions)
      )

  private def fuzzFunction1NWith4Parameters(
      fn: CHFunctionFuzzResult
  )(using client: CHClient, ec: ExecutionContext): Future[CHFunctionFuzzResult] =
    logger.debug("fuzzFunction1NWith4Parameters")
    if fn.parametric3Function1Ns.isEmpty then Future.successful(fn)
    else
      fuzzAddOneParameter(
        fn.name,
        fn.parametric3Function1Ns,
        (f: Parametric3Function1N, param: CHFuzzableType) =>
          CHFunctionIO.Parametric4Function1N(f.paramArg1, f.paramArg2, f.paramArg3, param, f.arg1, f.argN, f.output),
        (f: Parametric3Function1N, param: CHFuzzableType) =>
          CHFunctionIO.Parametric3NFunction1N(f.paramArg1, f.paramArg2, f.paramArg3, param, f.arg1, f.argN, f.output),
        (finiteFunctions: Seq[Parametric4Function1N]) => fn.copy(parametric4Function1Ns = finiteFunctions),
        (infiniteFunctions: Seq[Parametric3NFunction1N]) => fn.copy(parametric3NFunction1Ns = infiniteFunctions)
      )

  private def fuzzFunction2NWith2Or1NParameters(
      fn: CHFunctionFuzzResult
  )(using client: CHClient, ec: ExecutionContext): Future[CHFunctionFuzzResult] =
    logger.debug("fuzzFunction2NWith2Or1NParameters")
    if fn.parametric1Function2Ns.isEmpty then Future.successful(fn)
    else
      fuzzAddOneParameter(
        fn.name,
        fn.parametric1Function2Ns,
        (f: Parametric1Function2N, param: CHFuzzableType) =>
          CHFunctionIO.Parametric2Function2N(f.paramArg1, param, f.arg1, f.arg2, f.argN, f.output),
        (f: Parametric1Function2N, param: CHFuzzableType) =>
          CHFunctionIO.Parametric1NFunction2N(f.paramArg1, param, f.arg1, f.arg2, f.argN, f.output),
        (finiteFunctions: Seq[Parametric2Function2N]) => fn.copy(parametric2Function2Ns = finiteFunctions),
        (infiniteFunctions: Seq[Parametric1NFunction2N]) => fn.copy(parametric1NFunction2Ns = infiniteFunctions)
      )

  private def fuzzFunction2NWith3Parameters(
      fn: CHFunctionFuzzResult
  )(using client: CHClient, ec: ExecutionContext): Future[CHFunctionFuzzResult] =
    logger.debug("fuzzFunction2NWith3Parameters")
    if fn.parametric2Function2Ns.isEmpty then Future.successful(fn)
    else
      fuzzAddOneParameter(
        fn.name,
        fn.parametric2Function2Ns,
        (f: Parametric2Function2N, param: CHFuzzableType) =>
          CHFunctionIO.Parametric3Function2N(f.paramArg1, f.paramArg2, param, f.arg1, f.arg2, f.argN, f.output),
        (f: Parametric2Function2N, param: CHFuzzableType) =>
          CHFunctionIO.Parametric2NFunction2N(f.paramArg1, f.paramArg2, param, f.arg1, f.arg2, f.argN, f.output),
        (finiteFunctions: Seq[Parametric3Function2N]) => fn.copy(parametric3Function2Ns = finiteFunctions),
        (infiniteFunctions: Seq[Parametric2NFunction2N]) => fn.copy(parametric2NFunction2Ns = infiniteFunctions)
      )

  private def fuzzFunction2NWith4Parameters(
      fn: CHFunctionFuzzResult
  )(using client: CHClient, ec: ExecutionContext): Future[CHFunctionFuzzResult] =
    logger.debug("fuzzFunction2NWith4Parameters")
    if fn.parametric3Function2Ns.isEmpty then Future.successful(fn)
    else
      fuzzAddOneParameter(
        fn.name,
        fn.parametric3Function2Ns,
        (f: Parametric3Function2N, param: CHFuzzableType) =>
          CHFunctionIO.Parametric4Function2N(
            f.paramArg1,
            f.paramArg2,
            f.paramArg3,
            param,
            f.arg1,
            f.arg2,
            f.argN,
            f.output
          ),
        (f: Parametric3Function2N, param: CHFuzzableType) =>
          CHFunctionIO.Parametric3NFunction2N(
            f.paramArg1,
            f.paramArg2,
            f.paramArg3,
            param,
            f.arg1,
            f.arg2,
            f.argN,
            f.output
          ),
        (finiteFunctions: Seq[Parametric4Function2N]) => fn.copy(parametric4Function2Ns = finiteFunctions),
        (infiniteFunctions: Seq[Parametric3NFunction2N]) => fn.copy(parametric3NFunction2Ns = infiniteFunctions)
      )

  /**
    * Test all combination of parametric functions containing paramCount parameters and argCount arguments
    *
    * @param fnName Name of the function to fuzz
    * @return All combinations that worked and their output type
    */
  private def fuzzParametricFunction(
      fnName: String,
      paramCount: Int,
      argCount: Int
  )(
      using client: CHClient,
      ec: ExecutionContext
  ): Future[Seq[(ParametricFunctionInput, OutputType)]] =
    // Build all combinations of parametric fonction input having paramCount parameters and argCount arguments
    // Those combinations are described using AbstractTypes!
    // They are all used to query ClickHouse and we are retrieving here only the ones that succeeded.
    val validCHFuzzableAbstractTypeCombinationsF: Future[Seq[ParametricFunctionAbstractInput]] =
      executeInParallelOnlySuccess(
        crossJoin(
          generateCHFuzzableAbstractTypeCombinations(paramCount, parametricAbstractType),
          generateCHFuzzableAbstractTypeCombinations(argCount)
        ),
        (paramTypes: Seq[CHFuzzableAbstractType], nonParamTypes: Seq[CHFuzzableAbstractType]) => {
          executeInSequenceUntilSuccess(
            crossJoin(
              buildFuzzingValuesArgs(paramTypes.map(_.fuzzingValues)),
              buildFuzzingValuesArgs(nonParamTypes.map(_.fuzzingValues))
            ).map { (paramArgs, nonParamArgs) => s"SELECT toTypeName($fnName($paramArgs)($nonParamArgs))" },
            client.executeNoResult
          ).map(_ => (paramTypes, nonParamTypes))
        },
        maxConcurrency = Settings.ClickHouse.maxSupportedConcurrency
      )

    // Expand abstract types to retrieve all types combinations and their output
    validCHFuzzableAbstractTypeCombinationsF.flatMap:
      (validCHFuzzableAbstractTypeCombinations: Seq[ParametricFunctionAbstractInput]) =>
        if validCHFuzzableAbstractTypeCombinations.isEmpty then Future.successful(Nil)
        else
          assume(
            validCHFuzzableAbstractTypeCombinations
              .groupBy(_.parameters)
              .values
              .map( // For each combination of parameters, we retrieve all the kind of available arguments
                _.map(_.arguments)
              )
              .toSet
              .size == 1, // All combination of parameters should support exactly the same combinations of arguments.
            s"While fuzzing $fnName with $paramCount parameters and $argCount arguments," +
              s" found some combinations of parameters that does not support the same combinations of arguments."
          )

          // As we previously assessed arguments and parameters have no relationship whatsoever
          // We will now fuzz their exact type (non abstract) separately.
          val argumentsAndSqlQuery: Seq[(NonParametricArguments, Seq[String])] =
            validCHFuzzableAbstractTypeCombinations
              .groupBy(_.parameters)
              .values
              .head // We only need one kind of parameters!
              .flatMap { (input: ParametricFunctionAbstractInput) =>
                generateCHFuzzableTypeCombinations(input.arguments)
                  .map((input.parameters, _))
              }
              .map { (paramAbstractTypes, nonParamTypes) =>
                val queries = crossJoin(
                  buildFuzzingValuesArgs(paramAbstractTypes.map(_.fuzzingValues)),
                  buildFuzzingValuesArgs(nonParamTypes.map(_.fuzzingValues))
                ).map { (paramArgs, nonParamArgs) => s"SELECT toTypeName($fnName($paramArgs)($nonParamArgs))" }

                (nonParamTypes, queries)
              }
          val parametersAndSqlQuery: Seq[(ParametricArguments, Seq[String])] =
            validCHFuzzableAbstractTypeCombinations
              .groupBy(_.arguments)
              .values
              .head // We only need one kind of arguments!
              .flatMap { (input: ParametricFunctionAbstractInput) =>
                generateCHFuzzableTypeCombinations(input.parameters)
                  .map((_, input.arguments))
              }
              .map { (paramTypes, nonParamAbstractTypes) =>
                val queries = crossJoin(
                  buildFuzzingValuesArgs(paramTypes.map(_.fuzzingValues)),
                  buildFuzzingValuesArgs(nonParamAbstractTypes.map(_.fuzzingValues))
                ).map { (paramArgs, nonParamArgs) => s"SELECT toTypeName($fnName($paramArgs)($nonParamArgs))" }

                (paramTypes, queries)
              }

          for
            // Fuzz arguments
            outputTypeByArguments: Map[NonParametricArguments, OutputType] <-
              executeInParallelOnlySuccess(
                argumentsAndSqlQuery,
                (nonParamTypes, queries) =>
                  executeInSequenceOnlySuccess(queries, client.execute(_).map(_.data.head.head.asInstanceOf[String]))
                    .map(outputTypes => (nonParamTypes, outputTypes.reduce(Fuzzer.mergeOutputType))),
                maxConcurrency = Settings.ClickHouse.maxSupportedConcurrency
              ).map(_.toMap)

            // Fuzz parameters
            validParameters: Seq[ParametricArguments] <-
              executeInParallelOnlySuccess(
                parametersAndSqlQuery,
                (paramTypes, queries) =>
                  executeInSequenceUntilSuccess(queries, client.executeNoResult).map(_ => paramTypes),
                maxConcurrency = Settings.ClickHouse.maxSupportedConcurrency
              )
          yield {
            for
              parameters <- validParameters
              (arguments, outputType) <- outputTypeByArguments.toSeq
            yield {
              ((parameters, arguments), outputType)
            }
          }

  /**
    * For all parametric functions previously found, fuzz if we can add a an additional parameter.
    *
    * @param fnName Name of the method to fuzz
    * @param fnBaseFunctions Parametric functions previously found
    * @param fnConstructor Lambda that builds the new parametric function
    * @return A Future containing all new parametric function
    */
  private def fuzzAddOneParameter[T <: CHFunctionIO, U1 <: CHFunctionIO, U2 <: CHFunctionIO](
      fnName: String,
      fnBaseFunctions: Seq[T],
      fnConstructorSingleParameter: (T, CHFuzzableType) => U1,
      fnConstructorInfiniteParameter: (T, CHFuzzableType) => U2,
      resultUpdatorSingleParameter: (Seq[U1]) => CHFunctionFuzzResult,
      resultUpdatorInfiniteParameter: (Seq[U2]) => CHFunctionFuzzResult
  )(using client: CHClient, ec: ExecutionContext): Future[CHFunctionFuzzResult] =
    require(fnBaseFunctions.nonEmpty, s"Cannot try adding one parameter when no functions are provided")

    val sampleFunction = fnBaseFunctions.head

    val params = sampleFunction.parameters.map(_.asInstanceOf[CHFuzzableType])
    val args = sampleFunction.arguments.map(_.asInstanceOf[CHFuzzableType])
    testAddOneParameter(
      fnName,
      params,
      args
    ).recoverWith(err =>
      if sampleFunction.hasInfiniteArgument then
        testAddOneParameter(
          fnName,
          params,
          args :+ args.last
        )
      else Future.failed(err)
    ).map { (validParameters, isInfiniteParam) =>
      if isInfiniteParam then
        resultUpdatorInfiniteParameter(
          for
            param <- validParameters
            f <- fnBaseFunctions
          yield fnConstructorInfiniteParameter(f, param)
        )
      else
        resultUpdatorSingleParameter(
          for
            param <- validParameters
            f <- fnBaseFunctions
          yield fnConstructorSingleParameter(f, param)
        )
    }

  /**
    * The future generated by this function never fails.
    *
    * @return Future.successful(true) if last argument can be repeated many times, otherwise Future.successful(false)
    */
  private def testInfiniteArgsFunctions(
      fnName: String,
      inputTypes: ParametricFunctionInput
  )(using client: CHClient, ec: ExecutionContext): Future[Boolean] =
    val arguments: NonParametricArguments = inputTypes.arguments
    require(arguments.nonEmpty, "Expected at least one defined argument, but none found.")

    // We shouldn't go to high, to avoid the following error:
    // Maximum number of arguments for aggregate function with Nullable types is 8. (NUMBER_OF_ARGUMENTS_DOESNT_MATCH)
    val argNv1 = Range(0, 4).toSeq.map(_ => Seq(arguments.last.fuzzingValues.head))
    val argNv2 = Range(0, 5).toSeq.map(_ => Seq(arguments.last.fuzzingValues.head))

    val fuzzingValuesParams = buildFuzzingValuesArgs(inputTypes.parameters.map(_.fuzzingValues))
    val fuzzingValuesArgsV1 = buildFuzzingValuesArgs(arguments.map(_.fuzzingValues) ++ argNv1)
    val fuzzingValuesArgsV2 = buildFuzzingValuesArgs(arguments.map(_.fuzzingValues) ++ argNv2)

    executeInSequenceUntilSuccess(
      crossJoin(fuzzingValuesParams, fuzzingValuesArgsV1 ++ fuzzingValuesArgsV2).map { (params, args) =>
        s"SELECT toTypeName($fnName($params)($args))"
      },
      client.executeNoResult
    ).map(_ => true).recover(_ => false)

  /**
    * The future generated by this function never fails.
    *
    * @return Future.successful(true) if last parameter can be repeated many times, otherwise Future.successful(false)
    */
  private def testInfiniteParamsFunctions(
      fnName: String,
      inputTypes: ParametricFunctionInput
  )(using client: CHClient, ec: ExecutionContext): Future[Boolean] =
    val parameters: ParametricArguments = inputTypes.parameters
    require(parameters.nonEmpty, "Expected at least one defined parameter, but none found.")

    val paramN = Range(0, 5).toSeq.map(_ => Seq(parameters.last.fuzzingValues.head))

    val fuzzingValuesParams = buildFuzzingValuesArgs(parameters.map(_.fuzzingValues) ++ paramN)
    val fuzzingValuesArgs = buildFuzzingValuesArgs(inputTypes.arguments.map(_.fuzzingValues))

    executeInSequenceUntilSuccess(
      crossJoin(fuzzingValuesParams, fuzzingValuesArgs).map { (params, args) =>
        s"SELECT toTypeName($fnName($params)($args))"
      },
      client.executeNoResult
    ).map(_ => true).recover(_ => false)

  private def testAddOneParameter(
      fnName: String,
      currentParameters: ParametricArguments,
      arguments: NonParametricArguments
  )(using client: CHClient, ec: ExecutionContext): Future[(Seq[CHFuzzableType], Boolean)] =
    val additionalParamTypes = parametricAbstractType.flatMap(_.chFuzzableTypes)
    executeInParallelOnlySuccess(
      additionalParamTypes,
      additionalParamType => {
        val fuzzingValuesParams =
          buildFuzzingValuesArgs((currentParameters :+ additionalParamType).map(_.fuzzingValues))
        val fuzzingValuesArgs = buildFuzzingValuesArgs(arguments.map(_.fuzzingValues))

        executeInSequenceUntilSuccess(
          crossJoin(fuzzingValuesParams, fuzzingValuesArgs).map { (params, args) =>
            s"SELECT toTypeName($fnName($params)($args))"
          },
          client.executeNoResult
        ).map(_ => additionalParamType)
      },
      maxConcurrency = Settings.ClickHouse.maxSupportedConcurrency
    ).flatMap { validAdditionalParameters =>
      if validAdditionalParameters.isEmpty then Future.successful((Nil, false))
      else
        val sampleInput = (currentParameters :+ validAdditionalParameters.head, arguments)
        testInfiniteParamsFunctions(
          fnName,
          sampleInput
        ).map { isInfiniteParamsFunction => (validAdditionalParameters, isInfiniteParamsFunction) }
    }

  private def toFn[T <: CHFunctionIO](
      io: (ParametricFunctionInput, OutputType),
      parametricFunctionConstructor: (CHFuzzableType, CHFuzzableType, String) => T
  ): T =
    io._1 match
      case (Seq(param1), Seq(arg1)) => parametricFunctionConstructor(param1, arg1, io._2)
      case _ =>
        throw Exception(
          s"Expected 1 parameter and 1 argument, but found ${io._1.parameters.size} parameters and ${io._1.arguments.size} arguments"
        )

  private def toFn[T <: CHFunctionIO](
      io: (ParametricFunctionInput, OutputType),
      parametricFunctionConstructor: (CHFuzzableType, CHFuzzableType, CHFuzzableType, String) => T
  ): T =
    io._1 match
      case (Seq(param1), Seq(arg1, arg2)) => parametricFunctionConstructor(param1, arg1, arg2, io._2)
      case _ =>
        throw Exception(
          s"Expected 1 parameter and 2 arguments, but found ${io._1.parameters.size} parameters and ${io._1.arguments.size} arguments"
        )

  private def toFn[T <: CHFunctionIO](
      io: (ParametricFunctionInput, OutputType),
      parametricFunctionConstructor: (CHFuzzableType, CHFuzzableType, CHFuzzableType, CHFuzzableType, String) => T
  ): T =
    io._1 match
      case (Seq(param1), Seq(arg1, arg2, arg3)) => parametricFunctionConstructor(param1, arg1, arg2, arg3, io._2)
      case _ =>
        throw Exception(
          s"Expected 1 parameter and 3 arguments, but found ${io._1.parameters.size} parameters and ${io._1.arguments.size} arguments"
        )

  private type ParametricArguments = Seq[CHFuzzableType]
  private type NonParametricArguments = Seq[CHFuzzableType]
  private type OutputType = String

  opaque type ParametricFunctionInput = (Seq[CHFuzzableType], Seq[CHFuzzableType])
  extension (in: ParametricFunctionInput)
    @targetName("ParametricFunctionInput_parameters") def parameters: Seq[CHFuzzableType] = in._1
    @targetName("ParametricFunctionInput_arguments") def arguments: Seq[CHFuzzableType] = in._2

  opaque type ParametricFunctionAbstractInput = (Seq[CHFuzzableAbstractType], Seq[CHFuzzableAbstractType])
  extension (in: ParametricFunctionAbstractInput)
    @targetName("ParametricFunctionAbstractInput_parameters") def parameters: Seq[CHFuzzableAbstractType] = in._1
    @targetName("ParametricFunctionAbstractInput_arguments") def arguments: Seq[CHFuzzableAbstractType] = in._2
