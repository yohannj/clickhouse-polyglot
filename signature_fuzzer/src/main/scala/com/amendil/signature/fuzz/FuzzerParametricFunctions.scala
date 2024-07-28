package com.amendil.signature.fuzz

import com.amendil.common.entities.CHSettingWithValue
import com.amendil.common.entities.`type`.{CHFuzzableType, CHType}
import com.amendil.common.entities.function.{CHFunction, CHFunctionIO}
import com.amendil.common.entities.function.CHFunctionIO.*
import com.amendil.common.helper.*
import com.amendil.common.helper.ConcurrencyUtils.*
import com.amendil.common.http.CHClient
import com.amendil.signature.Settings
import com.amendil.signature.entities.*
import com.amendil.signature.fuzz.Fuzzer.*
import com.typesafe.scalalogging.StrictLogging

import scala.annotation.targetName
import scala.concurrent.{ExecutionContext, Future}
import scala.util.Success

object FuzzerParametricFunctions extends StrictLogging:

  // Remove some types that are obviously not parameters
  private[fuzz] val parametricAbstractType: Seq[CHFuzzableAbstractType] =
    CHFuzzableAbstractType.nonCustomFuzzableAbstractTypes ++
      CHFuzzableAbstractType.values.filter(_.isInstanceOf[ParametricCustomAbstractType])

  private[fuzz] def fuzzingFunctionWithCost(
      using client: CHClient,
      ec: ExecutionContext
  ): Seq[((CHFunctionFuzzResult) => Future[CHFunctionFuzzResult], Long)] =
    val paramCount = parametricAbstractType.flatMap(_.chFuzzableTypes).size.toLong
    val argCount = CHFuzzableType.values.size.toLong
    Seq(
      (fuzzFunction1Or0NWith0Parameter, argCount),
      (fuzzFunction2Or1NWith0Parameter, argCount * argCount),
      (fuzzFunction3Or2NWith0Parameter, argCount * argCount * argCount),
      (fuzzFunction1Or0NWith1Or0NParameter, paramCount * argCount),

      // Hack the cost to run at the same time as fuzzFunction1Or0NWith1Or0NParameter
      (fuzzFunction1Or0NWith2Or1NParameterForHardcodedMethods, paramCount * argCount),
      (fuzzFunction2Or1NWith1Or0NParameter, paramCount * argCount * argCount),
      // Functions below MUST happen after the "With1Or0NParameter", they are then very easy to compute
      (fuzzFunction1With2Or1NParameters, paramCount * argCount + 1),
      (fuzzFunction0NWith2Or1NParameters, paramCount * argCount + 1),
      (fuzzFunction2With2Or1NParameters, paramCount * argCount * argCount + 1),
      (fuzzFunction1NWith2Or1NParameters, paramCount * argCount * argCount + 1),
      // Functions below MUST happen after the "With2Or1NParameter", they are then very easy to compute
      (fuzzFunction1With3Or2NParameters, paramCount * argCount + 2),
      (fuzzFunction0NWith3Or2NParameters, paramCount * argCount + 2),
      (fuzzFunction2With3Parameters, paramCount * argCount * argCount + 2),
      (fuzzFunction1NWith3Or2NParameters, paramCount * argCount * argCount + 2)
      // Functions below MUST happen after the "With3Parameter", they are then very easy to compute
      // (fuzzFunction1With4Or3NParameters, paramCount * argCount + 3),
      // (fuzzFunction0NWith4Or3NParameters, paramCount * argCount + 3),
      // (fuzzFunction2With4Or3NParameters, paramCount * argCount * argCount + 3),
      // (fuzzFunction1NWith4Or3NParameters, paramCount * argCount * argCount + 3),
    )

  private def fuzzFunction1Or0NWith0Parameter(
      fn: CHFunctionFuzzResult
  )(using CHClient, ExecutionContext): Future[CHFunctionFuzzResult] =
    logger.debug("fuzzFunction1Or0NWith0Parameter")
    fuzzParametric(
      fn,
      paramCount = 0,
      argCount = 1,
      (io: (ParametricFunctionInput, OutputType)) => toFn(io, CHFunctionIO.Parametric0Function1.apply),
      (io: (ParametricFunctionInput, OutputType)) =>
        throw new IllegalArgumentException("No parameters provided, cannot have an repeated number of parameters."),
      (io: (ParametricFunctionInput, OutputType)) => toFn(io, CHFunctionIO.Parametric0Function0N.apply),
      (io: (ParametricFunctionInput, OutputType)) =>
        throw new IllegalArgumentException("No parameters provided, cannot have an repeated number of parameters.")
    ).map((modes, settings, p0Fn1s, _, p0Fn0Ns, _) =>
      logger.trace(s"fuzzFunction1Or0NWith0Parameter - fuzz done")
      fn.copy(
        modes = fn.modes ++ modes,
        settings = fn.settings ++ settings,
        parametric0Function1s = p0Fn1s,
        parametric0Function0Ns = p0Fn0Ns
      )
    )

  private def fuzzFunction2Or1NWith0Parameter(
      fn: CHFunctionFuzzResult
  )(using CHClient, ExecutionContext): Future[CHFunctionFuzzResult] =
    logger.debug("fuzzFunction2Or1NWith0Parameter")
    if fn.parametric0Function0Ns.nonEmpty || (fn.functions.exists(_.parameters.size > 0) && !fn.functions.exists(
        _.parameters.size <= 0
      ))
    then Future.successful(fn)
    else
      fuzzParametric(
        fn,
        paramCount = 0,
        argCount = 2,
        (io: (ParametricFunctionInput, OutputType)) => toFn(io, CHFunctionIO.Parametric0Function2.apply),
        (io: (ParametricFunctionInput, OutputType)) =>
          throw new IllegalArgumentException("No parameters provided, cannot have an repeated number of parameters."),
        (io: (ParametricFunctionInput, OutputType)) => toFn(io, CHFunctionIO.Parametric0Function1N.apply),
        (io: (ParametricFunctionInput, OutputType)) =>
          throw new IllegalArgumentException("No parameters provided, cannot have an repeated number of parameters.")
      ).map((modes, settings, p0Fn2s, _, p0Fn1Ns, _) =>
        logger.trace(s"fuzzFunction2Or1NWith0Parameter - fuzz done")
        fn.copy(
          modes = fn.modes ++ modes,
          settings = fn.settings ++ settings,
          parametric0Function2s = p0Fn2s,
          parametric0Function1Ns = p0Fn1Ns
        )
      )

  private def fuzzFunction3Or2NWith0Parameter(
      fn: CHFunctionFuzzResult
  )(using CHClient, ExecutionContext): Future[CHFunctionFuzzResult] =
    logger.debug("fuzzFunction3Or2NWith0Parameter")
    if fn.parametric0Function0Ns.nonEmpty || fn.parametric0Function1Ns.nonEmpty || (
        fn.parametric0Function1s.filterNot(_.arg1.name.startsWith("Tuple")).nonEmpty && fn.parametric0Function2s.isEmpty
      ) || (fn.functions.exists(_.parameters.size > 0) && !fn.functions.exists(_.parameters.size <= 0))
    then Future.successful(fn)
    else
      fuzzParametric(
        fn,
        paramCount = 0,
        argCount = 3,
        (io: (ParametricFunctionInput, OutputType)) => toFn(io, CHFunctionIO.Parametric0Function3.apply),
        (io: (ParametricFunctionInput, OutputType)) =>
          throw new IllegalArgumentException("No parameters provided, cannot have an repeated number of parameters."),
        (io: (ParametricFunctionInput, OutputType)) => toFn(io, CHFunctionIO.Parametric0Function2N.apply),
        (io: (ParametricFunctionInput, OutputType)) =>
          throw new IllegalArgumentException("No parameters provided, cannot have an repeated number of parameters.")
      ).map((modes, settings, p0Fn3s, _, p0Fn2Ns, _) =>
        logger.trace(s"fuzzFunction3Or2NWith0Parameter - fuzz done")
        fn.copy(
          modes = fn.modes ++ modes,
          settings = fn.settings ++ settings,
          parametric0Function3s = p0Fn3s,
          parametric0Function2Ns = p0Fn2Ns
        )
      )

  private def fuzzFunction1Or0NWith1Or0NParameter(
      fn: CHFunctionFuzzResult
  )(using CHClient, ExecutionContext): Future[CHFunctionFuzzResult] =
    logger.debug("fuzzFunction1Or0NWith1Or0NParameter")
    fuzzParametric(
      fn,
      paramCount = 1,
      argCount = 1,
      (io: (ParametricFunctionInput, OutputType)) => toFn(io, CHFunctionIO.Parametric1Function1.apply),
      (io: (ParametricFunctionInput, OutputType)) => toFn(io, CHFunctionIO.Parametric0NFunction1.apply),
      (io: (ParametricFunctionInput, OutputType)) => toFn(io, CHFunctionIO.Parametric1Function0N.apply),
      (io: (ParametricFunctionInput, OutputType)) => toFn(io, CHFunctionIO.Parametric0NFunction0N.apply)
    ).map((modes, settings, p1Fn1s, p0NFn1s, p1Fn0Ns, p0NFn0Ns) =>
      logger.trace(s"fuzzFunction1Or0NWith1Or0NParameter - fuzz done")
      fn.copy(
        modes = fn.modes ++ modes,
        settings = fn.settings ++ settings,
        parametric1Function1s = p1Fn1s,
        parametric0NFunction1s = p0NFn1s,
        parametric1Function0Ns = p1Fn0Ns,
        parametric0NFunction0Ns = p0NFn0Ns
      )
    )

  private def fuzzFunction1Or0NWith2Or1NParameterForHardcodedMethods(
      fn: CHFunctionFuzzResult
  )(using CHClient, ExecutionContext): Future[CHFunctionFuzzResult] =
    if Seq("quantilesDD", "quantilesGK").contains(fn.name) then
      logger.debug("fuzzFunction1Or0NWith2Or1NParameterForHardcodedMethods")
      fuzzParametric(
        fn,
        paramCount = 2,
        argCount = 1,
        (io: (ParametricFunctionInput, OutputType)) => toFn(io, CHFunctionIO.Parametric2Function1.apply),
        (io: (ParametricFunctionInput, OutputType)) => toFn(io, CHFunctionIO.Parametric1NFunction1.apply),
        (io: (ParametricFunctionInput, OutputType)) => toFn(io, CHFunctionIO.Parametric2Function0N.apply),
        (io: (ParametricFunctionInput, OutputType)) => toFn(io, CHFunctionIO.Parametric1NFunction0N.apply)
      ).map((modes, settings, p2Fn1s, p1NFn1s, p2Fn0Ns, p1NFn0Ns) =>
        logger.trace(s"${p2Fn1s.size}")
        logger.trace(s"${p1NFn1s.size}")
        logger.trace(s"${p2Fn0Ns.size}")
        logger.trace(s"${p1NFn0Ns.size}")
        logger.trace(s"fuzzFunction1Or0NWith2Or1NParameterForHardcodedMethods - fuzz done")
        fn.copy(
          modes = fn.modes ++ modes,
          settings = fn.settings ++ settings,
          parametric2Function1s = p2Fn1s,
          parametric1NFunction1s = p1NFn1s,
          parametric2Function0Ns = p2Fn0Ns,
          parametric1NFunction0Ns = p1NFn0Ns
        )
      )
    else Future.successful(fn)

  private def fuzzFunction2Or1NWith1Or0NParameter(
      fn: CHFunctionFuzzResult
  )(using CHClient, ExecutionContext): Future[CHFunctionFuzzResult] =
    logger.debug("fuzzFunction2Or1NWith1Or0NParameter")
    if fn.parametric0NFunction0Ns.nonEmpty || fn.parametric1Function0Ns.nonEmpty || (fn.functions.exists(
        _.parameters.size > 1
      ) && !fn.functions.exists(_.parameters.size <= 1))
    then Future.successful(fn)
    else
      fuzzParametric(
        fn,
        paramCount = 1,
        argCount = 2,
        (io: (ParametricFunctionInput, OutputType)) => toFn(io, CHFunctionIO.Parametric1Function2.apply),
        (io: (ParametricFunctionInput, OutputType)) => toFn(io, CHFunctionIO.Parametric0NFunction2.apply),
        (io: (ParametricFunctionInput, OutputType)) => toFn(io, CHFunctionIO.Parametric1Function1N.apply),
        (io: (ParametricFunctionInput, OutputType)) => toFn(io, CHFunctionIO.Parametric0NFunction1N.apply)
      ).map((modes, settings, p1Fn2s, p0NFn2s, p1Fn1Ns, p0NFn1Ns) =>
        logger.trace(s"fuzzFunction2Or1NWith1Or0NParameter - fuzz done")
        fn.copy(
          modes = fn.modes ++ modes,
          settings = fn.settings ++ settings,
          parametric1Function2s = p1Fn2s,
          parametric0NFunction2s = p0NFn2s,
          parametric1Function1Ns = p1Fn1Ns,
          parametric0NFunction1Ns = p0NFn1Ns
        )
      )

  private def fuzzFunction1With2Or1NParameters(
      fn: CHFunctionFuzzResult
  )(using CHClient, ExecutionContext): Future[CHFunctionFuzzResult] =
    logger.debug("fuzzFunction1With2Or1NParameters")
    if fn.parametric1Function1s.isEmpty then Future.successful(fn)
    else
      val fuzzOverWindow = !fn.modes.contains(CHFunction.Mode.NoOverWindow)
      fuzzAddOneParameter(
        fn.name,
        fuzzOverWindow,
        fn.parametric1Function1s,
        (f: Parametric1Function1, param: CHFuzzableType) =>
          CHFunctionIO.Parametric2Function1(f.paramArg1, param, f.arg1, f.output),
        (f: Parametric1Function1, param: CHFuzzableType) =>
          CHFunctionIO.Parametric1NFunction1(f.paramArg1, param, f.arg1, f.output),
        (finiteFunctions: Seq[Parametric2Function1]) => fn.copy(parametric2Function1s = finiteFunctions),
        (repeatedFunctions: Seq[Parametric1NFunction1]) => fn.copy(parametric1NFunction1s = repeatedFunctions)
      )

  private def fuzzFunction1With3Or2NParameters(
      fn: CHFunctionFuzzResult
  )(using CHClient, ExecutionContext): Future[CHFunctionFuzzResult] =
    logger.debug("fuzzFunction1With3Or2NParameters")
    if fn.parametric2Function1s.isEmpty then Future.successful(fn)
    else
      val fuzzOverWindow = !fn.modes.contains(CHFunction.Mode.NoOverWindow)
      fuzzAddOneParameter(
        fn.name,
        fuzzOverWindow,
        fn.parametric2Function1s,
        (f: Parametric2Function1, param: CHFuzzableType) =>
          CHFunctionIO.Parametric3Function1(f.paramArg1, f.paramArg2, param, f.arg1, f.output),
        (f: Parametric2Function1, param: CHFuzzableType) =>
          CHFunctionIO.Parametric2NFunction1(f.paramArg1, f.paramArg2, param, f.arg1, f.output),
        (finiteFunctions: Seq[Parametric3Function1]) => fn.copy(parametric3Function1s = finiteFunctions),
        (repeatedFunctions: Seq[Parametric2NFunction1]) => fn.copy(parametric2NFunction1s = repeatedFunctions)
      )

  private def fuzzFunction1With4Or3NParameters(
      fn: CHFunctionFuzzResult
  )(using CHClient, ExecutionContext): Future[CHFunctionFuzzResult] =
    logger.debug("fuzzFunction1With4Or3NParameters")
    if fn.parametric3Function1s.isEmpty then Future.successful(fn)
    else
      val fuzzOverWindow = !fn.modes.contains(CHFunction.Mode.NoOverWindow)
      fuzzAddOneParameter(
        fn.name,
        fuzzOverWindow,
        fn.parametric3Function1s,
        (f: Parametric3Function1, param: CHFuzzableType) =>
          CHFunctionIO.Parametric4Function1(f.paramArg1, f.paramArg2, f.paramArg3, param, f.arg1, f.output),
        (f: Parametric3Function1, param: CHFuzzableType) =>
          CHFunctionIO.Parametric3NFunction1(f.paramArg1, f.paramArg2, f.paramArg3, param, f.arg1, f.output),
        (finiteFunctions: Seq[Parametric4Function1]) => fn.copy(parametric4Function1s = finiteFunctions),
        (repeatedFunctions: Seq[Parametric3NFunction1]) => fn.copy(parametric3NFunction1s = repeatedFunctions)
      )

  private def fuzzFunction2With2Or1NParameters(
      fn: CHFunctionFuzzResult
  )(using CHClient, ExecutionContext): Future[CHFunctionFuzzResult] =
    logger.debug("fuzzFunction2With2Or1NParameters")
    if fn.parametric1Function2s.isEmpty then Future.successful(fn)
    else
      val fuzzOverWindow = !fn.modes.contains(CHFunction.Mode.NoOverWindow)
      fuzzAddOneParameter(
        fn.name,
        fuzzOverWindow,
        fn.parametric1Function2s,
        (f: Parametric1Function2, param: CHFuzzableType) =>
          CHFunctionIO.Parametric2Function2(f.paramArg1, param, f.arg1, f.arg2, f.output),
        (f: Parametric1Function2, param: CHFuzzableType) =>
          CHFunctionIO.Parametric1NFunction2(f.paramArg1, param, f.arg1, f.arg2, f.output),
        (finiteFunctions: Seq[Parametric2Function2]) => fn.copy(parametric2Function2s = finiteFunctions),
        (repeatedFunctions: Seq[Parametric1NFunction2]) => fn.copy(parametric1NFunction2s = repeatedFunctions)
      )

  private def fuzzFunction2With3Parameters(
      fn: CHFunctionFuzzResult
  )(using CHClient, ExecutionContext): Future[CHFunctionFuzzResult] =
    logger.debug("fuzzFunction2With3Parameters")
    if fn.parametric2Function2s.isEmpty then Future.successful(fn)
    else
      val fuzzOverWindow = !fn.modes.contains(CHFunction.Mode.NoOverWindow)
      fuzzAddOneParameter(
        fn.name,
        fuzzOverWindow,
        fn.parametric2Function2s,
        (f: Parametric2Function2, param: CHFuzzableType) =>
          CHFunctionIO.Parametric3Function2(f.paramArg1, f.paramArg2, param, f.arg1, f.arg2, f.output),
        (f: Parametric2Function2, param: CHFuzzableType) =>
          CHFunctionIO.Parametric2NFunction2(f.paramArg1, f.paramArg2, param, f.arg1, f.arg2, f.output),
        (finiteFunctions: Seq[Parametric3Function2]) => fn.copy(parametric3Function2s = finiteFunctions),
        (repeatedFunctions: Seq[Parametric2NFunction2]) => fn.copy(parametric2NFunction2s = repeatedFunctions)
      )

  private def fuzzFunction2With4Or3NParameters(
      fn: CHFunctionFuzzResult
  )(using CHClient, ExecutionContext): Future[CHFunctionFuzzResult] =
    logger.debug("fuzzFunction2With4Or3NParameters")
    if fn.parametric3Function2s.isEmpty then Future.successful(fn)
    else
      val fuzzOverWindow = !fn.modes.contains(CHFunction.Mode.NoOverWindow)
      fuzzAddOneParameter(
        fn.name,
        fuzzOverWindow,
        fn.parametric3Function2s,
        (f: Parametric3Function2, param: CHFuzzableType) =>
          CHFunctionIO.Parametric4Function2(f.paramArg1, f.paramArg2, f.paramArg3, param, f.arg1, f.arg2, f.output),
        (f: Parametric3Function2, param: CHFuzzableType) =>
          CHFunctionIO.Parametric3NFunction2(f.paramArg1, f.paramArg2, f.paramArg3, param, f.arg1, f.arg2, f.output),
        (finiteFunctions: Seq[Parametric4Function2]) => fn.copy(parametric4Function2s = finiteFunctions),
        (repeatedFunctions: Seq[Parametric3NFunction2]) => fn.copy(parametric3NFunction2s = repeatedFunctions)
      )

  private def fuzzFunction0NWith2Or1NParameters(
      fn: CHFunctionFuzzResult
  )(using CHClient, ExecutionContext): Future[CHFunctionFuzzResult] =
    logger.debug("fuzzFunction0NWith2Or1NParameters")
    if fn.parametric1Function0Ns.isEmpty then Future.successful(fn)
    else
      val fuzzOverWindow = !fn.modes.contains(CHFunction.Mode.NoOverWindow)
      fuzzAddOneParameter(
        fn.name,
        fuzzOverWindow,
        fn.parametric1Function0Ns,
        (f: Parametric1Function0N, param: CHFuzzableType) =>
          CHFunctionIO.Parametric2Function0N(f.paramArg1, param, f.argN, f.output),
        (f: Parametric1Function0N, param: CHFuzzableType) =>
          CHFunctionIO.Parametric1NFunction0N(f.paramArg1, param, f.argN, f.output),
        (finiteFunctions: Seq[Parametric2Function0N]) => fn.copy(parametric2Function0Ns = finiteFunctions),
        (repeatedFunctions: Seq[Parametric1NFunction0N]) => fn.copy(parametric1NFunction0Ns = repeatedFunctions)
      )

  private def fuzzFunction0NWith3Or2NParameters(
      fn: CHFunctionFuzzResult
  )(using CHClient, ExecutionContext): Future[CHFunctionFuzzResult] =
    logger.debug("fuzzFunction0NWith3Or2NParameters")
    if fn.parametric2Function0Ns.isEmpty then Future.successful(fn)
    else
      val fuzzOverWindow = !fn.modes.contains(CHFunction.Mode.NoOverWindow)
      fuzzAddOneParameter(
        fn.name,
        fuzzOverWindow,
        fn.parametric2Function0Ns,
        (f: Parametric2Function0N, param: CHFuzzableType) =>
          CHFunctionIO.Parametric3Function0N(f.paramArg1, f.paramArg2, param, f.argN, f.output),
        (f: Parametric2Function0N, param: CHFuzzableType) =>
          CHFunctionIO.Parametric2NFunction0N(f.paramArg1, f.paramArg2, param, f.argN, f.output),
        (finiteFunctions: Seq[Parametric3Function0N]) => fn.copy(parametric3Function0Ns = finiteFunctions),
        (repeatedFunctions: Seq[Parametric2NFunction0N]) => fn.copy(parametric2NFunction0Ns = repeatedFunctions)
      )

  private def fuzzFunction0NWith4Or3NParameters(
      fn: CHFunctionFuzzResult
  )(using CHClient, ExecutionContext): Future[CHFunctionFuzzResult] =
    logger.debug("fuzzFunction0NWith4Or3NParameters")
    if fn.parametric3Function0Ns.isEmpty then Future.successful(fn)
    else
      val fuzzOverWindow = !fn.modes.contains(CHFunction.Mode.NoOverWindow)
      fuzzAddOneParameter(
        fn.name,
        fuzzOverWindow,
        fn.parametric3Function0Ns,
        (f: Parametric3Function0N, param: CHFuzzableType) =>
          CHFunctionIO.Parametric4Function0N(f.paramArg1, f.paramArg2, f.paramArg3, param, f.argN, f.output),
        (f: Parametric3Function0N, param: CHFuzzableType) =>
          CHFunctionIO.Parametric3NFunction0N(f.paramArg1, f.paramArg2, f.paramArg3, param, f.argN, f.output),
        (finiteFunctions: Seq[Parametric4Function0N]) => fn.copy(parametric4Function0Ns = finiteFunctions),
        (repeatedFunctions: Seq[Parametric3NFunction0N]) => fn.copy(parametric3NFunction0Ns = repeatedFunctions)
      )

  private def fuzzFunction1NWith2Or1NParameters(
      fn: CHFunctionFuzzResult
  )(using CHClient, ExecutionContext): Future[CHFunctionFuzzResult] =
    logger.debug("fuzzFunction1NWith2Or1NParameters")
    if fn.parametric1Function1Ns.isEmpty then Future.successful(fn)
    else
      val fuzzOverWindow = !fn.modes.contains(CHFunction.Mode.NoOverWindow)
      fuzzAddOneParameter(
        fn.name,
        fuzzOverWindow,
        fn.parametric1Function1Ns,
        (f: Parametric1Function1N, param: CHFuzzableType) =>
          CHFunctionIO.Parametric2Function1N(f.paramArg1, param, f.arg1, f.argN, f.output),
        (f: Parametric1Function1N, param: CHFuzzableType) =>
          CHFunctionIO.Parametric1NFunction1N(f.paramArg1, param, f.arg1, f.argN, f.output),
        (finiteFunctions: Seq[Parametric2Function1N]) => fn.copy(parametric2Function1Ns = finiteFunctions),
        (repeatedFunctions: Seq[Parametric1NFunction1N]) => fn.copy(parametric1NFunction1Ns = repeatedFunctions)
      )

  private def fuzzFunction1NWith3Or2NParameters(
      fn: CHFunctionFuzzResult
  )(using CHClient, ExecutionContext): Future[CHFunctionFuzzResult] =
    logger.debug("fuzzFunction1NWith3Or2NParameters")
    if fn.parametric2Function1Ns.isEmpty then Future.successful(fn)
    else
      val fuzzOverWindow = !fn.modes.contains(CHFunction.Mode.NoOverWindow)
      fuzzAddOneParameter(
        fn.name,
        fuzzOverWindow,
        fn.parametric2Function1Ns,
        (f: Parametric2Function1N, param: CHFuzzableType) =>
          CHFunctionIO.Parametric3Function1N(f.paramArg1, f.paramArg2, param, f.arg1, f.argN, f.output),
        (f: Parametric2Function1N, param: CHFuzzableType) =>
          CHFunctionIO.Parametric2NFunction1N(f.paramArg1, f.paramArg2, param, f.arg1, f.argN, f.output),
        (finiteFunctions: Seq[Parametric3Function1N]) => fn.copy(parametric3Function1Ns = finiteFunctions),
        (repeatedFunctions: Seq[Parametric2NFunction1N]) => fn.copy(parametric2NFunction1Ns = repeatedFunctions)
      )

  private def fuzzFunction1NWith4Or3NParameters(
      fn: CHFunctionFuzzResult
  )(using CHClient, ExecutionContext): Future[CHFunctionFuzzResult] =
    logger.debug("fuzzFunction1NWith4Or3NParameters")
    if fn.parametric3Function1Ns.isEmpty then Future.successful(fn)
    else
      val fuzzOverWindow = !fn.modes.contains(CHFunction.Mode.NoOverWindow)
      fuzzAddOneParameter(
        fn.name,
        fuzzOverWindow,
        fn.parametric3Function1Ns,
        (f: Parametric3Function1N, param: CHFuzzableType) =>
          CHFunctionIO.Parametric4Function1N(f.paramArg1, f.paramArg2, f.paramArg3, param, f.arg1, f.argN, f.output),
        (f: Parametric3Function1N, param: CHFuzzableType) =>
          CHFunctionIO.Parametric3NFunction1N(f.paramArg1, f.paramArg2, f.paramArg3, param, f.arg1, f.argN, f.output),
        (finiteFunctions: Seq[Parametric4Function1N]) => fn.copy(parametric4Function1Ns = finiteFunctions),
        (repeatedFunctions: Seq[Parametric3NFunction1N]) => fn.copy(parametric3NFunction1Ns = repeatedFunctions)
      )

  private def fuzzParametric[U1 <: CHFunctionIO, U2 <: CHFunctionIO, U3 <: CHFunctionIO, U4 <: CHFunctionIO](
      fn: CHFunctionFuzzResult,
      paramCount: Int,
      argCount: Int,
      fnConstructorFiniteParamsFiniteArgs: ((ParametricFunctionInput, OutputType)) => U1,
      fnConstructorRepeatedParamsFiniteArgs: ((ParametricFunctionInput, OutputType)) => U2,
      fnConstructorFiniteParamsRepeatedArgs: ((ParametricFunctionInput, OutputType)) => U3,
      fnConstructorRepeatedParamsRepeatedArgs: ((ParametricFunctionInput, OutputType)) => U4
  )(
      using client: CHClient,
      ec: ExecutionContext
  ): Future[(Set[CHFunction.Mode], Set[CHSettingWithValue[Boolean]], Seq[U1], Seq[U2], Seq[U3], Seq[U4])] =
    val skipFuzzingF =
      if Settings.Fuzzer.skipFuzzingOnArgumentMismatch
      then checkArgMismatch(fn.name, paramCount, argCount)
      else Future.successful(false)

    skipFuzzingF.flatMap(skipFuzzing =>
      if skipFuzzing then Future.successful((Set.empty, Set.empty, Nil, Nil, Nil, Nil))
      else if fn.atLeastOneSignatureFound then
        // We know whether the function requires/supports or not `OVER window` so we can bruteforce only one of the valid values
        fuzzParametricSingleMode(
          fn.name,
          paramCount,
          argCount,
          fuzzOverWindow = !fn.modes.contains(CHFunction.Mode.NoOverWindow),
          fnConstructorFiniteParamsFiniteArgs,
          fnConstructorRepeatedParamsFiniteArgs,
          fnConstructorFiniteParamsRepeatedArgs,
          fnConstructorRepeatedParamsRepeatedArgs
        ).map(
            // format: off
            (finiteParamsFiniteArgsFn, repeatedParamsFiniteArgsFn, finiteParamsRepeatedArgsFn, repeatedParamsRepeatedArgsFn) =>
              (Set.empty, Set.empty, finiteParamsFiniteArgsFn, repeatedParamsFiniteArgsFn, finiteParamsRepeatedArgsFn, repeatedParamsRepeatedArgsFn)
            // format: on
        )
      else
        // format: off
        // We don't yet know if the function requires/supports or not `OVER window` so we need to brute force them all
        // First check without OVER window
        fuzzParametricSingleMode(fn.name, paramCount, argCount, fuzzOverWindow = false, fnConstructorFiniteParamsFiniteArgs, fnConstructorRepeatedParamsFiniteArgs, fnConstructorFiniteParamsRepeatedArgs, fnConstructorRepeatedParamsRepeatedArgs)
          .transformWith {
            case Success((finiteParamsFiniteArgsFn, repeatedParamsFiniteArgsFn, finiteParamsRepeatedArgsFn, repeatedParamsRepeatedArgsFn))
              if finiteParamsFiniteArgsFn.nonEmpty || repeatedParamsFiniteArgsFn.nonEmpty ||
                finiteParamsRepeatedArgsFn.nonEmpty || repeatedParamsRepeatedArgsFn.nonEmpty =>
                // Success without OVER window, let's try a sample function with OVER window
                val sampleFn =
                  finiteParamsFiniteArgsFn.headOption
                    .orElse(repeatedParamsFiniteArgsFn.headOption)
                    .orElse(finiteParamsRepeatedArgsFn.headOption)
                    .orElse(repeatedParamsRepeatedArgsFn.headOption)
                    .get

                testSampleFunctionWithOverWindow(fn.name, sampleFn).map( supportOverWindow =>
                  if supportOverWindow
                  then (Set(CHFunction.Mode.OverWindow, CHFunction.Mode.NoOverWindow), finiteParamsFiniteArgsFn, repeatedParamsFiniteArgsFn, finiteParamsRepeatedArgsFn, repeatedParamsRepeatedArgsFn)
                  else (Set(CHFunction.Mode.NoOverWindow), finiteParamsFiniteArgsFn, repeatedParamsFiniteArgsFn, finiteParamsRepeatedArgsFn, repeatedParamsRepeatedArgsFn)
                )

            case _ =>
              // Failure without OVER window, fuzz with OVER window
              fuzzParametricSingleMode(fn.name, paramCount, argCount, fuzzOverWindow = true, fnConstructorFiniteParamsFiniteArgs, fnConstructorRepeatedParamsFiniteArgs, fnConstructorFiniteParamsRepeatedArgs, fnConstructorRepeatedParamsRepeatedArgs).map(
                (finiteParamsFiniteArgsFn, repeatedParamsFiniteArgsFn, finiteParamsRepeatedArgsFn, repeatedParamsRepeatedArgsFn) =>
                  (Set(CHFunction.Mode.OverWindow), finiteParamsFiniteArgsFn, repeatedParamsFiniteArgsFn, finiteParamsRepeatedArgsFn, repeatedParamsRepeatedArgsFn)
              ).recover(_ => (Set.empty, Nil, Nil, Nil, Nil)) // Nothing worked
          }
          .transformWith{
            case Success((modes, finiteParamsFiniteArgsFn, repeatedParamsFiniteArgsFn, finiteParamsRepeatedArgsFn, repeatedParamsRepeatedArgsFn))
              if finiteParamsFiniteArgsFn.nonEmpty || repeatedParamsFiniteArgsFn.nonEmpty ||
                finiteParamsRepeatedArgsFn.nonEmpty || repeatedParamsRepeatedArgsFn.nonEmpty =>

              val sampleFn =
                finiteParamsFiniteArgsFn.headOption
                  .orElse(repeatedParamsFiniteArgsFn.headOption)
                  .orElse(finiteParamsRepeatedArgsFn.headOption)
                  .orElse(repeatedParamsRepeatedArgsFn.headOption)
                  .get

              detectMandatorySettingsFromSampleFunction(fn.name, sampleFn, fuzzOverWindow = !(fn.modes ++ modes).contains(CHFunction.Mode.NoOverWindow))
                .map(settings => (modes, settings, finiteParamsFiniteArgsFn, repeatedParamsFiniteArgsFn, finiteParamsRepeatedArgsFn, repeatedParamsRepeatedArgsFn))

            case _ => Future.successful((Set.empty, Set.empty, Nil, Nil, Nil, Nil))
          }
        // format: on
    )

  private def fuzzParametricSingleMode[U1 <: CHFunctionIO, U2 <: CHFunctionIO, U3 <: CHFunctionIO, U4 <: CHFunctionIO](
      fnName: String,
      paramCount: Int,
      argCount: Int,
      fuzzOverWindow: Boolean,
      fnConstructorFiniteParamsFiniteArgs: ((ParametricFunctionInput, OutputType)) => U1,
      fnConstructorRepeatedParamsFiniteArgs: ((ParametricFunctionInput, OutputType)) => U2,
      fnConstructorFiniteParamsRepeatedArgs: ((ParametricFunctionInput, OutputType)) => U3,
      fnConstructorRepeatedParamsRepeatedArgs: ((ParametricFunctionInput, OutputType)) => U4
  )(using CHClient, ExecutionContext): Future[(Seq[U1], Seq[U2], Seq[U3], Seq[U4])] =
    for
      functions: Seq[(ParametricFunctionInput, OutputType)] <-
        fuzzFiniteParamsAndArgsFunction(fnName, paramCount, argCount, fuzzOverWindow)

      fnHasRepeatedParams: Boolean <-
        if functions.isEmpty || paramCount == 0 then Future.successful(false)
        else
          testRepeatedParamsFunctions(
            fnName,
            sampleInput = functions.head._1,
            lastParameterTypes = functions.map(_._1.parameters.last).toSet,
            fuzzOverWindow
          )

      fnHasRepeatedArgs: Boolean <-
        if functions.isEmpty || argCount == 0 then Future.successful(false)
        else
          testRepeatedArgsFunctions(
            fnName,
            sampleInput = functions.head._1,
            lastArgumentTypes = functions.map(_._1.arguments.last).toSet,
            fuzzOverWindow
          )
    yield
      if fnHasRepeatedParams && fnHasRepeatedArgs then
        (Nil, Nil, Nil, functions.map(fnConstructorRepeatedParamsRepeatedArgs))
      else if fnHasRepeatedParams && !fnHasRepeatedArgs then
        (Nil, functions.map(fnConstructorRepeatedParamsFiniteArgs), Nil, Nil)
      else if !fnHasRepeatedParams && fnHasRepeatedArgs then
        (Nil, Nil, functions.map(fnConstructorFiniteParamsRepeatedArgs), Nil)
      else (functions.map(fnConstructorFiniteParamsFiniteArgs), Nil, Nil, Nil)

  /**
    * Test all combination of parametric functions containing paramCount parameters and argCount arguments
    *
    * @param fnName Name of the function to fuzz
    * @return All combinations that worked and their output type
    */
  private def fuzzFiniteParamsAndArgsFunction(
      fnName: String,
      paramCount: Int,
      argCount: Int,
      fuzzOverWindow: Boolean
  )(
      using client: CHClient,
      ec: ExecutionContext
  ): Future[Seq[(ParametricFunctionInput, OutputType)]] =
    for
      // Build all combinations of parametric function input having paramCount parameters and argCount arguments
      // Those combinations are described using AbstractTypes!
      // They are all used to query ClickHouse and we are retrieving here only the ones that succeeded.
      validCHFuzzableAbstractTypeCombinations: Seq[ParametricFunctionAbstractInput] <-
        getValidAbstractTypeCombinations(fnName, paramCount, argCount, fuzzOverWindow)

      // Expand abstract types to retrieve all types combinations and their output
      abstractInputCombinations <- fuzzAbstractInputCombinations(
        fnName,
        validCHFuzzableAbstractTypeCombinations,
        fuzzOverWindow
      )
    yield abstractInputCombinations

  private[fuzz] def fuzzAbstractInputCombinations(
      fnName: String,
      abstractInputCombinations: Seq[(Seq[CHFuzzableAbstractType], Seq[CHFuzzableAbstractType])],
      fuzzOverWindow: Boolean
  )(
      using client: CHClient,
      ec: ExecutionContext
  ): Future[Seq[((Seq[CHFuzzableType], Seq[CHFuzzableType]), OutputType)]] =
    // Expand abstract types to retrieve all types combinations and their output
    logger.trace(
      s"fuzzFiniteParamsAndArgsFunction - detected ${abstractInputCombinations.size} abstract input combinations"
    )
    if abstractInputCombinations.isEmpty then Future.successful(Nil)
    else
      assume(
        abstractInputCombinations
          .groupBy(_.parameters)
          .values
          .map( // For each combination of parameters, we retrieve all the kind of available arguments
            _.map(_.arguments)
          )
          .toSet
          .size == 1, // All combination of parameters should support exactly the same combinations of arguments.
        s"While fuzzing $fnName with ${abstractInputCombinations.head._1.size} parameters and" +
          s" ${abstractInputCombinations.head._1.size} arguments, found some combinations of parameters" +
          s" that does not support the same combinations of arguments."
      )

      // As we previously assessed arguments and parameters have no relationship whatsoever
      // We will now fuzz their exact type (non abstract) separately.
      val argumentsAndSqlQuery: Seq[(NonParametricArguments, Seq[String])] =
        abstractInputCombinations
          .groupBy(_.parameters)
          .values
          .head // We only need one kind of parameters!
          .flatMap { (input: ParametricFunctionAbstractInput) =>
            generateCHFuzzableTypeCombinations(input.arguments)
              .map((input.parameters, _))
          }
          .map { (paramAbstractTypes, nonParamTypes) =>
            val queries = crossJoin(
              buildFuzzingValuesArgs(paramAbstractTypes.map(_.exhaustiveFuzzingValues)),
              buildFuzzingValuesArgs(nonParamTypes.map(_.fuzzingValues))
            ).map { (paramArgs, nonParamArgs) => parametricQuery(fnName, paramArgs, nonParamArgs, fuzzOverWindow) }

            (nonParamTypes, queries)
          }
      val parametersAndSqlQuery: Seq[(ParametricArguments, Seq[String])] =
        abstractInputCombinations
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
              buildFuzzingValuesArgs(nonParamAbstractTypes.map(_.exhaustiveFuzzingValues))
            ).map { (paramArgs, nonParamArgs) => parametricQuery(fnName, paramArgs, nonParamArgs, fuzzOverWindow) }

            (paramTypes, queries)
          }

      logger.trace(
        s"fuzzFiniteParamsAndArgsFunction - testing ${argumentsAndSqlQuery.size} arguments combinations and " +
          s"${parametersAndSqlQuery.size} parameters combinations"
      )

      for
        // Fuzz arguments
        outputTypeByArguments: Map[NonParametricArguments, OutputType] <-
          executeInParallelOnlySuccess(
            argumentsAndSqlQuery,
            (nonParamTypes, queries) =>
              executeInSequenceOnlySuccess(queries, client.execute(_).map(_.data.head.head.asInstanceOf[String]))
                .map(outputTypes =>
                  (nonParamTypes, outputTypes.map(CHTypeParser.getByName).reduce(CHTypeMerger.mergeOutputType))
                ),
            maxConcurrency = Settings.ClickHouse.maxSupportedConcurrency
          ).map(_.toMap)

        _ = logger.trace(
          s"fuzzFiniteParamsAndArgsFunction - ${outputTypeByArguments.size} arguments combinations are valid"
        )

        // Fuzz parameters
        validParameters: Seq[ParametricArguments] <-
          executeInParallelOnlySuccess(
            parametersAndSqlQuery,
            (
                paramTypes,
                queries
            ) => executeInSequenceUntilSuccess(queries, client.executeNoResult(_)).map(_ => paramTypes),
            maxConcurrency = Settings.ClickHouse.maxSupportedConcurrency
          )

        _ = logger.trace(
          s"fuzzFiniteParamsAndArgsFunction - ${validParameters.size} parameters combinations are valid"
        )
      yield for
        parameters <- validParameters
        (arguments, outputType) <- outputTypeByArguments.toSeq
      yield ((parameters, arguments), outputType)

  /**
    * Build all combinations of function input having argCount arguments
    * Those combinations are described using AbstractTypes!
    *
    * Then bruteforce them to find the valid ones.
    */
  private def getValidAbstractTypeCombinations(
      fnName: String,
      paramCount: Int,
      argCount: Int,
      fuzzOverWindow: Boolean
  )(using CHClient, ExecutionContext): Future[Seq[ParametricFunctionAbstractInput]] =
    for
      // For performance reasons, we first exclude custom types that also work as String.
      abstractInputCombinationsNoSpecialType: Seq[ParametricFunctionAbstractInput] <-
        bruteforceAbstractTypeCombinations(
          crossJoin(
            generateCHFuzzableAbstractTypeCombinations(paramCount, parametricAbstractType),
            generateCHFuzzableAbstractTypeCombinations(argCount)
          ).filterNot((paramTypes, argTypes) =>
            paramTypes.exists(_.isInstanceOf[CustomAbstractType]) ||
              argTypes.exists(_.isInstanceOf[CustomAbstractType])
          ),
          fnName,
          fuzzOverWindow
        )

      _ = logger.trace(
        s"getValidAbstractTypeCombinations - detected ${abstractInputCombinationsNoSpecialType.size} abstract input combinations (excluding special types)"
      )

      // If nothing was found previously, then it might be because we need a custom types.
      abstractInputCombinations: Seq[ParametricFunctionAbstractInput] <-
        if abstractInputCombinationsNoSpecialType.nonEmpty then
          Future.successful(abstractInputCombinationsNoSpecialType)
        else
          bruteforceAbstractTypeCombinations(
            crossJoin(
              generateCHFuzzableAbstractTypeCombinations(paramCount, parametricAbstractType),
              generateCHFuzzableAbstractTypeCombinations(argCount)
            ).filter((paramTypes, argTypes) =>
              paramTypes.exists(_.isInstanceOf[CustomAbstractType]) ||
                argTypes.exists(_.isInstanceOf[CustomAbstractType])
            ),
            fnName,
            fuzzOverWindow
          )

      _ = logger.trace(
        s"getValidAbstractTypeCombinations - detected ${abstractInputCombinations.size} abstract input combinations"
      )
    yield abstractInputCombinations

  /**
    * Query ClickHouse for all given combinations and we returning those that succeeded at least once.
    */
  private def bruteforceAbstractTypeCombinations(
      combinations: Seq[(Seq[CHFuzzableAbstractType], Seq[CHFuzzableAbstractType])],
      fnName: String,
      fuzzOverWindow: Boolean
  )(using client: CHClient, ec: ExecutionContext): Future[Seq[ParametricFunctionAbstractInput]] =
    executeInParallelOnlySuccess(
      combinations,
      (paramTypes: Seq[CHFuzzableAbstractType], nonParamTypes: Seq[CHFuzzableAbstractType]) =>
        executeInParallelUntilSuccess(
          crossJoin(
            buildFuzzingValuesArgs(paramTypes.map(_.fuzzingValues)),
            buildFuzzingValuesArgs(nonParamTypes.map(_.fuzzingValues))
          ).map { (paramArgs, nonParamArgs) => parametricQuery(fnName, paramArgs, nonParamArgs, fuzzOverWindow) },
          client.executeNoResult(_),
          maxConcurrency = Settings.ClickHouse.maxSupportedConcurrencyInnerLoop
        ).map(_ => (paramTypes, nonParamTypes)),
      maxConcurrency = Settings.ClickHouse.maxSupportedConcurrencyOuterLoop
    )

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
      fuzzOverWindow: Boolean,
      fnBaseFunctions: Seq[T],
      fnConstructorFiniteParams: (T, CHFuzzableType) => U1,
      fnConstructorRepeatedParams: (T, CHFuzzableType) => U2,
      resultUpdatorSingleParameter: (Seq[U1]) => CHFunctionFuzzResult,
      resultUpdatorRepeatedParameter: (Seq[U2]) => CHFunctionFuzzResult
  )(using CHClient, ExecutionContext): Future[CHFunctionFuzzResult] =
    require(fnBaseFunctions.nonEmpty, s"Cannot try adding one parameter when no functions are provided")

    val sampleFunction = fnBaseFunctions.head

    val params = sampleFunction.parameters.map(_.asInstanceOf[CHFuzzableType])
    val args = sampleFunction.arguments.map(_.asInstanceOf[CHFuzzableType])
    testAddOneParameter(fnName, params, args, fuzzOverWindow)
      .recoverWith(err =>
        if sampleFunction.repeatedArgumentIdxOpt.nonEmpty then
          testAddOneParameter(fnName, params, args :+ args.last, fuzzOverWindow)
        else Future.failed(err)
      )
      .map { (validParameters, isRepeatedParam) =>
        if isRepeatedParam then
          resultUpdatorRepeatedParameter(
            for
              param <- validParameters
              f <- fnBaseFunctions
            yield fnConstructorRepeatedParams(f, param)
          )
        else
          resultUpdatorSingleParameter(
            for
              param <- validParameters
              f <- fnBaseFunctions
            yield fnConstructorFiniteParams(f, param)
          )
      }

  /**
    * The future generated by this function never fails.
    *
    * @return Future.successful(true) if last argument can be repeated many times, otherwise Future.successful(false)
    */
  private def testRepeatedArgsFunctions(
      fnName: String,
      sampleInput: ParametricFunctionInput,
      lastArgumentTypes: Set[CHFuzzableType],
      fuzzOverWindow: Boolean
  )(using client: CHClient, ec: ExecutionContext): Future[Boolean] =
    val arguments: NonParametricArguments = sampleInput.arguments
    require(arguments.nonEmpty, "Expected at least one defined argument, but none found.")

    val fuzzingValuesParams = buildFuzzingValuesArgs(sampleInput.parameters.map(_.fuzzingValues))
    def buildArgs(lastType: CHFuzzableType): Seq[String] =
      // We shouldn't go to high, to avoid the following error:
      // Maximum number of arguments for aggregate function with Nullable types is 8. (NUMBER_OF_ARGUMENTS_DOESNT_MATCH)
      val argNv1 =
        Range(0, 3).toSeq.map(_ => Seq(arguments.last.fuzzingValues.head)) :+ Seq(lastType.fuzzingValues.head)
      val argNv2 =
        Range(0, 4).toSeq.map(_ => Seq(arguments.last.fuzzingValues.head)) :+ Seq(lastType.fuzzingValues.head)

      val fuzzingValuesParams = buildFuzzingValuesArgs(sampleInput.parameters.map(_.fuzzingValues))
      val fuzzingValuesArgsV1 = buildFuzzingValuesArgs(arguments.map(_.fuzzingValues) ++ argNv1)
      val fuzzingValuesArgsV2 = buildFuzzingValuesArgs(arguments.map(_.fuzzingValues) ++ argNv2)

      fuzzingValuesArgsV1 ++ fuzzingValuesArgsV2

    val resF =
      for
        // Try repeating the last argument N times
        _ <-
          executeInParallelUntilSuccess(
            crossJoin(fuzzingValuesParams, buildArgs(arguments.last)).map { (params, args) =>
              parametricQuery(fnName, params, args, fuzzOverWindow)
            },
            client.executeNoResult(_),
            Settings.ClickHouse.maxSupportedConcurrency
          )

        otherValidTypes <-
          executeInParallelOnlySuccess(
            // Test all chType that were not found as valid for current argument
            CHFuzzableAbstractType.nonCustomFuzzableTypes.filterNot(lastArgumentTypes),
            chFuzzableType =>
              executeInParallelUntilSuccess(
                crossJoin(fuzzingValuesParams, buildArgs(chFuzzableType)).map { (params, args) =>
                  parametricQuery(fnName, params, args, fuzzOverWindow)
                },
                client.executeNoResult(_),
                Settings.ClickHouse.maxSupportedConcurrencyInnerLoop
              ).map(_ => chFuzzableType),
            Settings.ClickHouse.maxSupportedConcurrencyOuterLoop
          )
      yield otherValidTypes.isEmpty // If no other types are valid, then current argument is the repeated one.

    resF.recover(_ => false)

  /**
    * The future generated by this function never fails.
    *
    * @return Future.successful(true) if last parameter can be repeated many times, otherwise Future.successful(false)
    */
  private def testRepeatedParamsFunctions(
      fnName: String,
      sampleInput: ParametricFunctionInput,
      lastParameterTypes: Set[CHFuzzableType],
      fuzzOverWindow: Boolean
  )(using client: CHClient, ec: ExecutionContext): Future[Boolean] =
    val parameters: ParametricArguments = sampleInput.parameters
    require(parameters.nonEmpty, "Expected at least one defined parameter, but none found.")

    val fuzzingValuesArgs = buildFuzzingValuesArgs(sampleInput.arguments.map(_.fuzzingValues))
    def buildParams(lastType: CHFuzzableType): Seq[String] =
      val paramN =
        Range(0, 4).toSeq.map(_ => Seq(parameters.last.fuzzingValues.head)) :+ Seq(lastType.fuzzingValues.head)
      buildFuzzingValuesArgs(parameters.map(_.fuzzingValues) ++ paramN)

    val resF =
      for
        // Try repeating the last argument N times
        _ <-
          executeInParallelUntilSuccess(
            crossJoin(buildParams(parameters.last), fuzzingValuesArgs).map { (params, args) =>
              parametricQuery(fnName, params, args, fuzzOverWindow)
            },
            client.executeNoResult(_),
            Settings.ClickHouse.maxSupportedConcurrency
          )

        otherValidTypes <-
          executeInParallelOnlySuccess(
            // Test all chType that were not found as valid for current argument
            CHFuzzableAbstractType.nonCustomFuzzableTypes.filterNot(lastParameterTypes),
            chFuzzableType =>
              executeInParallelUntilSuccess(
                crossJoin(buildParams(chFuzzableType), fuzzingValuesArgs).map { (params, args) =>
                  parametricQuery(fnName, params, args, fuzzOverWindow)
                },
                client.executeNoResult(_),
                Settings.ClickHouse.maxSupportedConcurrencyInnerLoop
              ).map(_ => chFuzzableType),
            Settings.ClickHouse.maxSupportedConcurrencyOuterLoop
          )
      yield otherValidTypes.isEmpty // If no other types are valid, then current argument is the repeated one.

    resF.recover(_ => false)

  private def testAddOneParameter(
      fnName: String,
      currentParameters: ParametricArguments,
      arguments: NonParametricArguments,
      fuzzOverWindow: Boolean
  )(using client: CHClient, ec: ExecutionContext): Future[(Seq[CHFuzzableType], Boolean)] =
    val additionalParamTypes = parametricAbstractType.flatMap(_.chFuzzableTypes)
    executeInParallelOnlySuccess(
      additionalParamTypes,
      additionalParamType =>
        val fuzzingValuesParams =
          buildFuzzingValuesArgs((currentParameters :+ additionalParamType).map(_.fuzzingValues))
        val fuzzingValuesArgs = buildFuzzingValuesArgs(arguments.map(_.fuzzingValues))

        executeInSequenceUntilSuccess(
          crossJoin(fuzzingValuesParams, fuzzingValuesArgs).map { (params, args) =>
            parametricQuery(fnName, params, args, fuzzOverWindow)
          },
          client.executeNoResult(_)
        ).map(_ => additionalParamType)
      ,
      maxConcurrency = Settings.ClickHouse.maxSupportedConcurrency
    ).flatMap { validAdditionalParameters =>
      if validAdditionalParameters.isEmpty then Future.successful((Nil, false))
      else
        val sampleInput = (currentParameters :+ validAdditionalParameters.head, arguments)
        testRepeatedParamsFunctions(
          fnName,
          sampleInput = sampleInput,
          lastParameterTypes = validAdditionalParameters.toSet,
          fuzzOverWindow
        ).map { isRepeatedParamsFunction => (validAdditionalParameters, isRepeatedParamsFunction) }
    }

  /**
    * @return true if a NUMBER_OF_ARGUMENTS_DOESNT_MATCH error was returned by ClickHouse, false otherwise
    */
  private def checkArgMismatch(fnName: String, paramCount: Int, argCount: Int)(
      using client: CHClient,
      ec: ExecutionContext
  ): Future[Boolean] =
    if fnName.equals("ntile") && argCount != 1 then Future.successful(true)
    else
      client
        .executeNoResult(
          parametricQuery(fnName, Range(0, paramCount).mkString(", "), Range(0, argCount).mkString(", "), false)
        )
        .map(_ => false)
        .recover { err =>
          Seq(
            "(TOO_FEW_ARGUMENTS_FOR_FUNCTION)",
            "(NUMBER_OF_ARGUMENTS_DOESNT_MATCH)",
            "(TOO_MANY_ARGUMENTS_FOR_FUNCTION)"
          ).exists(err.getMessage().contains)
        }

  private def toFn[T <: CHFunctionIO](
      io: (ParametricFunctionInput, OutputType),
      parametricFunctionConstructor: (CHFuzzableType, CHType) => T
  ): T =
    io._1 match
      case (Seq(), Seq(arg1)) => parametricFunctionConstructor(arg1, io._2)
      case _ =>
        val msg =
          s"Expected 0 parameter and 1 argument, but found ${io._1.parameters.size} parameters and ${io._1.arguments.size} arguments"
        logger.error(msg)
        throw Exception(msg)

  private def toFn[T <: CHFunctionIO](
      io: (ParametricFunctionInput, OutputType),
      parametricFunctionConstructor: (CHFuzzableType, CHFuzzableType, CHType) => T
  ): T =
    io._1 match
      case (Seq(), Seq(arg1, arg2)) => parametricFunctionConstructor(arg1, arg2, io._2)
      case (Seq(param1), Seq(arg1)) => parametricFunctionConstructor(param1, arg1, io._2)
      case _ =>
        val msg =
          s"Expected a total of 2 parameters and arguments, but found ${io._1.parameters.size} parameters and ${io._1.arguments.size} arguments"
        logger.error(msg)
        throw Exception(msg)

  private def toFn[T <: CHFunctionIO](
      io: (ParametricFunctionInput, OutputType),
      parametricFunctionConstructor: (CHFuzzableType, CHFuzzableType, CHFuzzableType, CHType) => T
  ): T =
    io._1 match
      case (Seq(), Seq(arg1, arg2, arg3))   => parametricFunctionConstructor(arg1, arg2, arg3, io._2)
      case (Seq(param1), Seq(arg1, arg2))   => parametricFunctionConstructor(param1, arg1, arg2, io._2)
      case (Seq(param1, param2), Seq(arg1)) => parametricFunctionConstructor(param1, param2, arg1, io._2)
      case _ =>
        val msg =
          s"Expected a total of 3 parameters and arguments, but found ${io._1.parameters.size} parameters and ${io._1.arguments.size} arguments"
        logger.error(msg)
        throw Exception(msg)

  private def toFn[T <: CHFunctionIO](
      io: (ParametricFunctionInput, OutputType),
      parametricFunctionConstructor: (CHFuzzableType, CHFuzzableType, CHFuzzableType, CHFuzzableType, CHType) => T
  ): T =
    io._1 match
      case (Seq(param1), Seq(arg1, arg2, arg3)) => parametricFunctionConstructor(param1, arg1, arg2, arg3, io._2)
      case _ =>
        val msg =
          s"Expected a total of 4 parameters and arguments, but found ${io._1.parameters.size} parameters and ${io._1.arguments.size} arguments"
        logger.error(msg)
        throw Exception(msg)

  private type ParametricArguments = Seq[CHFuzzableType]
  private type NonParametricArguments = Seq[CHFuzzableType]
  private type OutputType = CHType

  opaque type ParametricFunctionInput = (Seq[CHFuzzableType], Seq[CHFuzzableType])
  extension (in: ParametricFunctionInput)
    @targetName("ParametricFunctionInput_parameters") def parameters: Seq[CHFuzzableType] = in._1
    @targetName("ParametricFunctionInput_arguments") def arguments: Seq[CHFuzzableType] = in._2

  opaque type ParametricFunctionAbstractInput = (Seq[CHFuzzableAbstractType], Seq[CHFuzzableAbstractType])
  extension (in: ParametricFunctionAbstractInput)
    @targetName("ParametricFunctionAbstractInput_parameters") def parameters: Seq[CHFuzzableAbstractType] = in._1
    @targetName("ParametricFunctionAbstractInput_arguments") def arguments: Seq[CHFuzzableAbstractType] = in._2
