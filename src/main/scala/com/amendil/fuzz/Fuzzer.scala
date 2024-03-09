package com.amendil.fuzz

import com.amendil.entities._
import com.amendil.http.CHClient
import com.typesafe.scalalogging.StrictLogging

import scala.concurrent.{ExecutionContext, Future}

object Fuzzer extends StrictLogging {
  def fuzz(functionName: String)(implicit client: CHClient, ec: ExecutionContext): Future[CHFunctionFuzzResult] =
    // TODO OPTIMIZE function check order!!!
    // 18:46:25.654 [pool-1-thread-1] INFO Main -- ===============================================================
    // 18:46:25.658 [pool-1-thread-1] INFO Main -- 0%
    // 18:46:25.661 [pool-1-thread-1] INFO Main -- ===============================================================
    // 18:46:25.661 [pool-1-thread-1] INFO Main -- CAST
    // 19:09:05.722 [pool-1-thread-1] INFO Main -- ===============================================================
    // 19:09:05.722 [pool-1-thread-1] INFO Main -- 33%
    // 19:09:05.722 [pool-1-thread-1] INFO Main -- ===============================================================
    // 19:09:05.722 [pool-1-thread-1] INFO Main -- histogram
    // 19:35:39.891 [pool-1-thread-1] INFO Main -- ===============================================================
    // 19:35:39.891 [pool-1-thread-1] INFO Main -- 66%
    // 19:35:39.891 [pool-1-thread-1] INFO Main -- ===============================================================
    // 19:35:39.891 [pool-1-thread-1] INFO Main -- windowFunnel
    FuzzerSpecialFunctions
      .fuzz(CHFunctionFuzzResult(name = functionName))
      .flatMap(FuzzerNonParametricFunctions.fuzz)
      .flatMap(FuzzerParametricFunctions.fuzz)

  /**
    * @param chAbstractTypeList List of CHAbstractType that will be used to generate the combinations
    * @return All combinations of CHAbstractType of argCount elements.
    */
  private[fuzz] def generateCHAbstractTypeCombinations(
      argCount: Int,
      chAbstractTypeList: Seq[CHAbstractType] = CHAbstractType.values.toSeq
  ): Seq[Seq[CHAbstractType]] =
    generateCHAbstractTypeCombinations(argCount = argCount, currentArgs = Nil, chAbstractTypeList = chAbstractTypeList)

  private def generateCHAbstractTypeCombinations(
      argCount: Int,
      currentArgs: Seq[CHAbstractType],
      chAbstractTypeList: Seq[CHAbstractType]
  ): Seq[Seq[CHAbstractType]] =
    if (argCount > 0) {
      chAbstractTypeList.toSeq.map { abstractType =>
        generateCHAbstractTypeCombinations(argCount - 1, currentArgs :+ abstractType, chAbstractTypeList)
      }.flatten
    } else {
      Seq(currentArgs)
    }

  private[fuzz] def generateCHTypeCombinations(
      abstractTypes: Seq[CHAbstractType],
      currentArgs: Seq[CHType] = Seq.empty
  ): Seq[Seq[CHType]] =
    abstractTypes match {
      case Seq(head, tail @ _*) =>
        head.chTypes
          .map(chType => generateCHTypeCombinations(tail, currentArgs :+ chType))
          .flatten
      case Seq() => Seq(currentArgs)
    }

  private[fuzz] def buildFuzzingValuesArgs(argumentsValues: Seq[Seq[String]]): Seq[String] =
    argumentsValues match
      case Seq()   => throw IllegalArgumentException("Tried to fuzz an argument without any value")
      case Seq(el) => el
      case Seq(head, tail @ _*) =>
        val subChoices = buildFuzzingValuesArgs(tail)
        head.flatMap(el => subChoices.map(subChoice => s"$el, $subChoice"))

}
