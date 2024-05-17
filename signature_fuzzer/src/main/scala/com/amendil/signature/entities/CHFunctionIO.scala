package com.amendil.signature.entities

import com.amendil.common.entities.*

sealed trait CHFunctionIO:
  def kind: String
  def parameters: Seq[CHType] = Nil
  def arguments: Seq[CHType] = Nil
  def hasInfiniteParameter: Boolean = false
  def hasInfiniteArgument: Boolean = false
  def isParametric: Boolean = false
  def output: CHType

  def asString(): String =
    val parametersStr = parameters.map(_.name).mkString(", ") + (if hasInfiniteParameter then "..." else "")
    val argumentsStr = arguments.map(_.name).mkString(", ") + (if hasInfiniteArgument then "..." else "")

    if isParametric then s"($parametersStr)($argumentsStr) => ${output.name}".stripMargin
    else s"($argumentsStr) => ${output.name}".stripMargin

sealed trait CHFunctionNIO extends CHFunctionIO:
  override def hasInfiniteArgument: Boolean = true

sealed trait CHParametricFunction extends CHFunctionIO:
  override def isParametric: Boolean = true

sealed trait CHParametricNFunctionNIO extends CHParametricFunction:
  override def hasInfiniteParameter: Boolean = true
  override def hasInfiniteArgument: Boolean = true

sealed trait CHParametricNFunctionIO extends CHParametricFunction:
  override def hasInfiniteParameter: Boolean = true

sealed trait CHParametricFunctionNIO extends CHParametricFunction:
  override def hasInfiniteArgument: Boolean = true

object CHFunctionIO:

  def aggregate[T <: CHFunctionIO](functions: Seq[CHFunctionIO]): Seq[CHFunctionIO] =
    functions

  case class Function0N(argN: CHType, output: CHType) extends CHFunctionNIO:
    val kind = "function0N"
    override val arguments = Seq(argN)

  case class Function1N(arg1: CHType, argN: CHType, output: CHType) extends CHFunctionNIO:
    val kind = "function1N"
    override val arguments = Seq(arg1, argN)

  case class Function2N(arg1: CHType, arg2: CHType, argN: CHType, output: CHType) extends CHFunctionNIO:
    val kind = "function2N"
    override val arguments = Seq(arg1, arg2, argN)

  case class Function3N(arg1: CHType, arg2: CHType, arg3: CHType, argN: CHType, output: CHType) extends CHFunctionNIO:
    val kind = "function3N"
    override val arguments = Seq(arg1, arg2, arg3, argN)

  case class Function0(output: CHType) extends CHFunctionIO:
    val kind = "function0"

  case class Function1(arg1: CHType, output: CHType) extends CHFunctionIO:
    val kind = "function1"
    override val arguments = Seq(arg1)

  case class Function2(arg1: CHType, arg2: CHType, output: CHType) extends CHFunctionIO:
    val kind = "function2"
    override val arguments = Seq(arg1, arg2)

  case class Function3(arg1: CHType, arg2: CHType, arg3: CHType, output: CHType) extends CHFunctionIO:
    val kind = "function3"
    override val arguments = Seq(arg1, arg2, arg3)

  case class Function4(arg1: CHType, arg2: CHType, arg3: CHType, arg4: CHType, output: CHType) extends CHFunctionIO:
    val kind = "function4"
    override val arguments = Seq(arg1, arg2, arg3, arg4)

  case class Function5(arg1: CHType, arg2: CHType, arg3: CHType, arg4: CHType, arg5: CHType, output: CHType)
      extends CHFunctionIO:
    val kind = "function5"
    override val arguments = Seq(arg1, arg2, arg3, arg4, arg5)

  case class Function6(
      arg1: CHType,
      arg2: CHType,
      arg3: CHType,
      arg4: CHType,
      arg5: CHType,
      arg6: CHType,
      output: CHType
  ) extends CHFunctionIO:
    val kind = "function6"
    override val arguments = Seq(arg1, arg2, arg3, arg4, arg5, arg6)

  case class Function7(
      arg1: CHType,
      arg2: CHType,
      arg3: CHType,
      arg4: CHType,
      arg5: CHType,
      arg6: CHType,
      arg7: CHType,
      output: CHType
  ) extends CHFunctionIO:
    val kind = "function7"
    override val arguments = Seq(arg1, arg2, arg3, arg4, arg5, arg6, arg7)

  case class Function8(
      arg1: CHType,
      arg2: CHType,
      arg3: CHType,
      arg4: CHType,
      arg5: CHType,
      arg6: CHType,
      arg7: CHType,
      arg8: CHType,
      output: CHType
  ) extends CHFunctionIO:
    val kind = "function8"
    override val arguments = Seq(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8)

  case class Function9(
      arg1: CHType,
      arg2: CHType,
      arg3: CHType,
      arg4: CHType,
      arg5: CHType,
      arg6: CHType,
      arg7: CHType,
      arg8: CHType,
      arg9: CHType,
      output: CHType
  ) extends CHFunctionIO:
    val kind = "function9"
    override val arguments = Seq(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9)

  case class LambdaArrayFunction0N(lambdaArg: CHSpecialType.LambdaType, argN: CHSpecialType.Array, output: CHType)
      extends CHFunctionNIO:
    val kind = "lambdaArrayFunction0N"
    override val arguments = Seq(lambdaArg, argN)

  case class LambdaArrayFunction1N(
      lambdaArg: CHSpecialType.LambdaType,
      arg1: CHSpecialType.Array,
      argN: CHSpecialType.Array,
      output: CHType
  ) extends CHFunctionNIO:
    val kind = "lambdaArrayFunction1N"
    override val arguments = Seq(lambdaArg, arg1, argN)

  case class LambdaMapFunction1(lambdaArg: CHSpecialType.LambdaType, arg1: CHSpecialType.Map, output: CHType)
      extends CHFunctionIO:
    val kind = "LambdaMapFunction1"
    override val arguments = Seq(lambdaArg, arg1)

  case class Parametric0NFunction0N(paramArgN: CHType, argN: CHType, output: CHType) extends CHParametricNFunctionNIO:
    val kind = "parametric0NFunction0N"
    override val parameters = Seq(paramArgN)
    override val arguments = Seq(argN)

  case class Parametric0NFunction1N(paramArgN: CHType, arg1: CHType, argN: CHType, output: CHType)
      extends CHParametricNFunctionNIO:
    val kind = "parametric0NFunction1N"
    override val parameters = Seq(paramArgN)
    override val arguments = Seq(arg1, argN)

  case class Parametric0NFunction2N(paramArgN: CHType, arg1: CHType, arg2: CHType, argN: CHType, output: CHType)
      extends CHParametricNFunctionNIO:
    val kind = "parametric0NFunction2N"
    override val parameters = Seq(paramArgN)
    override val arguments = Seq(arg1, arg2, argN)

  case class Parametric0NFunction1(paramArgN: CHType, arg1: CHType, output: CHType) extends CHParametricNFunctionIO:
    val kind = "parametric0NFunction1"
    override val parameters = Seq(paramArgN)
    override val arguments = Seq(arg1)

  case class Parametric0NFunction2(paramArgN: CHType, arg1: CHType, arg2: CHType, output: CHType)
      extends CHParametricNFunctionIO:
    val kind = "parametric0NFunction2"
    override val parameters = Seq(paramArgN)
    override val arguments = Seq(arg1, arg2)

  case class Parametric0NFunction3(paramArgN: CHType, arg1: CHType, arg2: CHType, arg3: CHType, output: CHType)
      extends CHParametricNFunctionIO:
    val kind = "parametric0NFunction3"
    override val parameters = Seq(paramArgN)
    override val arguments = Seq(arg1, arg2, arg3)

  case class Parametric1NFunction0N(paramArg1: CHType, paramArgN: CHType, argN: CHType, output: CHType)
      extends CHParametricNFunctionNIO:
    val kind = "parametric1NFunction0N"
    override val parameters = Seq(paramArg1, paramArgN)
    override val arguments = Seq(argN)

  case class Parametric1NFunction1N(
      paramArg1: CHType,
      paramArgN: CHType,
      arg1: CHType,
      argN: CHType,
      output: CHType
  ) extends CHParametricNFunctionNIO:
    val kind = "parametric1NFunction1N"
    override val parameters = Seq(paramArg1, paramArgN)
    override val arguments = Seq(arg1, argN)

  case class Parametric1NFunction2N(
      paramArg1: CHType,
      paramArgN: CHType,
      arg1: CHType,
      arg2: CHType,
      argN: CHType,
      output: CHType
  ) extends CHParametricNFunctionNIO:
    val kind = "parametric1NFunction2N"
    override val parameters = Seq(paramArg1, paramArgN)
    override val arguments = Seq(arg1, arg2, argN)

  case class Parametric1NFunction3N(
      paramArg1: CHType,
      paramArgN: CHType,
      arg1: CHType,
      arg2: CHType,
      arg3: CHType,
      argN: CHType,
      output: CHType
  ) extends CHParametricNFunctionNIO:
    val kind = "parametric1NFunction3N"
    override val parameters = Seq(paramArg1, paramArgN)
    override val arguments = Seq(arg1, arg2, arg3, argN)

  case class Parametric1NFunction1(paramArg1: CHType, paramArgN: CHType, arg1: CHType, output: CHType)
      extends CHParametricNFunctionIO:
    val kind = "parametric1NFunction1"
    override val parameters = Seq(paramArg1, paramArgN)
    override val arguments = Seq(arg1)

  case class Parametric1NFunction2(
      paramArg1: CHType,
      paramArgN: CHType,
      arg1: CHType,
      arg2: CHType,
      output: CHType
  ) extends CHParametricNFunctionIO:
    val kind = "parametric1NFunction2"
    override val parameters = Seq(paramArg1, paramArgN)
    override val arguments = Seq(arg1, arg2)

  case class Parametric1NFunction3(
      paramArg1: CHType,
      paramArgN: CHType,
      arg1: CHType,
      arg2: CHType,
      arg3: CHType,
      output: CHType
  ) extends CHParametricNFunctionIO:
    val kind = "parametric1NFunction3"
    override val parameters = Seq(paramArg1, paramArgN)
    override val arguments = Seq(arg1, arg2, arg3)

  case class Parametric2NFunction0N(
      paramArg1: CHType,
      paramArg2: CHType,
      paramArgN: CHType,
      argN: CHType,
      output: CHType
  ) extends CHParametricNFunctionNIO:
    val kind = "parametric2NFunction0N"
    override val parameters = Seq(paramArg1, paramArg2, paramArgN)
    override val arguments = Seq(argN)

  case class Parametric2NFunction1N(
      paramArg1: CHType,
      paramArg2: CHType,
      paramArgN: CHType,
      arg1: CHType,
      argN: CHType,
      output: CHType
  ) extends CHParametricNFunctionNIO:
    val kind = "parametric2NFunction1N"
    override val parameters = Seq(paramArg1, paramArg2, paramArgN)
    override val arguments = Seq(arg1, argN)

  case class Parametric2NFunction2N(
      paramArg1: CHType,
      paramArg2: CHType,
      paramArgN: CHType,
      arg1: CHType,
      arg2: CHType,
      argN: CHType,
      output: CHType
  ) extends CHParametricNFunctionNIO:
    val kind = "parametric2NFunction2N"
    override val parameters = Seq(paramArg1, paramArg2, paramArgN)
    override val arguments = Seq(arg1, arg2, argN)

  case class Parametric2NFunction1(
      paramArg1: CHType,
      paramArg2: CHType,
      paramArgN: CHType,
      arg1: CHType,
      output: CHType
  ) extends CHParametricNFunctionIO:
    val kind = "parametric2NFunction1"
    override val parameters = Seq(paramArg1, paramArg2, paramArgN)
    override val arguments = Seq(arg1)

  case class Parametric2NFunction2(
      paramArg1: CHType,
      paramArg2: CHType,
      paramArgN: CHType,
      arg1: CHType,
      arg2: CHType,
      output: CHType
  ) extends CHParametricNFunctionIO:
    val kind = "parametric2NFunction2"
    override val parameters = Seq(paramArg1, paramArg2, paramArgN)
    override val arguments = Seq(arg1, arg2)

  case class Parametric2NFunction3(
      paramArg1: CHType,
      paramArg2: CHType,
      paramArgN: CHType,
      arg1: CHType,
      arg2: CHType,
      arg3: CHType,
      output: CHType
  ) extends CHParametricNFunctionIO:
    val kind = "parametric2NFunction3"
    override val parameters = Seq(paramArg1, paramArg2, paramArgN)
    override val arguments = Seq(arg1, arg2, arg3)

  case class Parametric3NFunction0N(
      paramArg1: CHType,
      paramArg2: CHType,
      paramArg3: CHType,
      paramArgN: CHType,
      argN: CHType,
      output: CHType
  ) extends CHParametricNFunctionNIO:
    val kind = "parametric3NFunction0N"
    override val parameters = Seq(paramArg1, paramArg2, paramArg3, paramArgN)
    override val arguments = Seq(argN)

  case class Parametric3NFunction1N(
      paramArg1: CHType,
      paramArg2: CHType,
      paramArg3: CHType,
      paramArgN: CHType,
      arg1: CHType,
      argN: CHType,
      output: CHType
  ) extends CHParametricNFunctionNIO:
    val kind = "parametric3NFunction1N"
    override val parameters = Seq(paramArg1, paramArg2, paramArg3, paramArgN)
    override val arguments = Seq(arg1, argN)

  case class Parametric3NFunction2N(
      paramArg1: CHType,
      paramArg2: CHType,
      paramArg3: CHType,
      paramArgN: CHType,
      arg1: CHType,
      arg2: CHType,
      argN: CHType,
      output: CHType
  ) extends CHParametricNFunctionNIO:
    val kind = "parametric3NFunction2N"
    override val parameters = Seq(paramArg1, paramArg2, paramArg3, paramArgN)
    override val arguments = Seq(arg1, arg2, argN)

  case class Parametric3NFunction1(
      paramArg1: CHType,
      paramArg2: CHType,
      paramArg3: CHType,
      paramArgN: CHType,
      arg1: CHType,
      output: CHType
  ) extends CHParametricNFunctionIO:
    val kind = "parametric3NFunction1"
    override val parameters = Seq(paramArg1, paramArg2, paramArg3, paramArgN)
    override val arguments = Seq(arg1)

  case class Parametric3NFunction2(
      paramArg1: CHType,
      paramArg2: CHType,
      paramArg3: CHType,
      paramArgN: CHType,
      arg1: CHType,
      arg2: CHType,
      output: CHType
  ) extends CHParametricNFunctionIO:
    val kind = "parametric3NFunction2"
    override val parameters = Seq(paramArg1, paramArg2, paramArg3, paramArgN)
    override val arguments = Seq(arg1, arg2)

  case class Parametric3NFunction3(
      paramArg1: CHType,
      paramArg2: CHType,
      paramArg3: CHType,
      paramArgN: CHType,
      arg1: CHType,
      arg2: CHType,
      arg3: CHType,
      output: CHType
  ) extends CHParametricNFunctionIO:
    val kind = "parametric3NFunction3"
    override val parameters = Seq(paramArg1, paramArg2, paramArg3, paramArgN)
    override val arguments = Seq(arg1, arg2, arg3)

  case class Parametric0Function0N(argN: CHType, output: CHType) extends CHParametricFunctionNIO:
    val kind = "parametric0Function0N"
    override val parameters = Seq()
    override val arguments = Seq(argN)

  case class Parametric0Function1N(arg1: CHType, argN: CHType, output: CHType) extends CHParametricFunctionNIO:
    val kind = "parametric0Function1N"
    override val parameters = Seq()
    override val arguments = Seq(arg1, argN)

  case class Parametric0Function2N(arg1: CHType, arg2: CHType, argN: CHType, output: CHType)
      extends CHParametricFunctionNIO:
    val kind = "parametric0Function2N"
    override val parameters = Seq()
    override val arguments = Seq(arg1, arg2, argN)

  case class Parametric0Function1(arg1: CHType, output: CHType) extends CHParametricFunction:
    val kind = "parametric0Function1"
    override val parameters = Seq()
    override val arguments = Seq(arg1)

  case class Parametric0Function2(arg1: CHType, arg2: CHType, output: CHType) extends CHParametricFunction:
    val kind = "parametric0Function2"
    override val parameters = Seq()
    override val arguments = Seq(arg1, arg2)

  case class Parametric0Function3(arg1: CHType, arg2: CHType, arg3: CHType, output: CHType)
      extends CHParametricFunction:
    val kind = "parametric0Function3"
    override val parameters = Seq()
    override val arguments = Seq(arg1, arg2, arg3)

  case class Parametric1Function0N(paramArg1: CHType, argN: CHType, output: CHType) extends CHParametricFunctionNIO:
    val kind = "parametric1Function0N"
    override val parameters = Seq(paramArg1)
    override val arguments = Seq(argN)

  case class Parametric1Function1N(paramArg1: CHType, arg1: CHType, argN: CHType, output: CHType)
      extends CHParametricFunctionNIO:
    val kind = "parametric1Function1N"
    override val parameters = Seq(paramArg1)
    override val arguments = Seq(arg1, argN)

  case class Parametric1Function2N(paramArg1: CHType, arg1: CHType, arg2: CHType, argN: CHType, output: CHType)
      extends CHParametricFunctionNIO:
    val kind = "parametric1Function2N"
    override val parameters = Seq(paramArg1)
    override val arguments = Seq(arg1, arg2, argN)

  case class Parametric1Function1(paramArg1: CHType, arg1: CHType, output: CHType) extends CHParametricFunction:
    val kind = "parametric1Function1"
    override val parameters = Seq(paramArg1)
    override val arguments = Seq(arg1)

  case class Parametric1Function2(paramArg1: CHType, arg1: CHType, arg2: CHType, output: CHType)
      extends CHParametricFunction:
    val kind = "parametric1Function2"
    override val parameters = Seq(paramArg1)
    override val arguments = Seq(arg1, arg2)

  case class Parametric1Function3(paramArg1: CHType, arg1: CHType, arg2: CHType, arg3: CHType, output: CHType)
      extends CHParametricFunction:
    val kind = "parametric1Function3"
    override val parameters = Seq(paramArg1)
    override val arguments = Seq(arg1, arg2, arg3)

  case class Parametric2Function0N(paramArg1: CHType, paramArg2: CHType, argN: CHType, output: CHType)
      extends CHParametricFunctionNIO:
    val kind = "parametric2Function0N"
    override val parameters = Seq(paramArg1, paramArg2)
    override val arguments = Seq(argN)

  case class Parametric2Function1N(
      paramArg1: CHType,
      paramArg2: CHType,
      arg1: CHType,
      argN: CHType,
      output: CHType
  ) extends CHParametricFunctionNIO:
    val kind = "parametric2Function1N"
    override val parameters = Seq(paramArg1, paramArg2)
    override val arguments = Seq(arg1, argN)

  case class Parametric2Function2N(
      paramArg1: CHType,
      paramArg2: CHType,
      arg1: CHType,
      arg2: CHType,
      argN: CHType,
      output: CHType
  ) extends CHParametricFunctionNIO:
    val kind = "parametric2Function2N"
    override val parameters = Seq(paramArg1, paramArg2)
    override val arguments = Seq(arg1, arg2, argN)

  case class Parametric2Function3N(
      paramArg1: CHType,
      paramArg2: CHType,
      arg1: CHType,
      arg2: CHType,
      arg3: CHType,
      argN: CHType,
      output: CHType
  ) extends CHParametricFunctionNIO:
    val kind = "parametric2Function3N"
    override val parameters = Seq(paramArg1, paramArg2)
    override val arguments = Seq(arg1, arg2, arg3, argN)

  case class Parametric2Function1(paramArg1: CHType, paramArg2: CHType, arg1: CHType, output: CHType)
      extends CHParametricFunction:
    val kind = "parametric2Function1"
    override val parameters = Seq(paramArg1, paramArg2)
    override val arguments = Seq(arg1)

  case class Parametric2Function2(
      paramArg1: CHType,
      paramArg2: CHType,
      arg1: CHType,
      arg2: CHType,
      output: CHType
  ) extends CHParametricFunction:
    val kind = "parametric2Function2"
    override val parameters = Seq(paramArg1, paramArg2)
    override val arguments = Seq(arg1, arg2)

  case class Parametric2Function3(
      paramArg1: CHType,
      paramArg2: CHType,
      arg1: CHType,
      arg2: CHType,
      arg3: CHType,
      output: CHType
  ) extends CHParametricFunction:
    val kind = "parametric2Function3"
    override val parameters = Seq(paramArg1, paramArg2)
    override val arguments = Seq(arg1, arg2, arg3)

  case class Parametric3Function0N(
      paramArg1: CHType,
      paramArg2: CHType,
      paramArg3: CHType,
      argN: CHType,
      output: CHType
  ) extends CHParametricFunctionNIO:
    val kind = "parametric3Function0N"
    override val parameters = Seq(paramArg1, paramArg2, paramArg3)
    override val arguments = Seq(argN)

  case class Parametric3Function1N(
      paramArg1: CHType,
      paramArg2: CHType,
      paramArg3: CHType,
      arg1: CHType,
      argN: CHType,
      output: CHType
  ) extends CHParametricFunctionNIO:
    val kind = "parametric3Function1N"
    override val parameters = Seq(paramArg1, paramArg2, paramArg3)
    override val arguments = Seq(arg1, argN)

  case class Parametric3Function2N(
      paramArg1: CHType,
      paramArg2: CHType,
      paramArg3: CHType,
      arg1: CHType,
      arg2: CHType,
      argN: CHType,
      output: CHType
  ) extends CHParametricFunctionNIO:
    val kind = "parametric3Function2N"
    override val parameters = Seq(paramArg1, paramArg2, paramArg3)
    override val arguments = Seq(arg1, arg2, argN)

  case class Parametric3Function1(
      paramArg1: CHType,
      paramArg2: CHType,
      paramArg3: CHType,
      arg1: CHType,
      output: CHType
  ) extends CHParametricFunction:
    val kind = "parametric3Function1"
    override val parameters = Seq(paramArg1, paramArg2, paramArg3)
    override val arguments = Seq(arg1)

  case class Parametric3Function2(
      paramArg1: CHType,
      paramArg2: CHType,
      paramArg3: CHType,
      arg1: CHType,
      arg2: CHType,
      output: CHType
  ) extends CHParametricFunction:
    val kind = "parametric3Function2"
    override val parameters = Seq(paramArg1, paramArg2, paramArg3)
    override val arguments = Seq(arg1, arg2)

  case class Parametric3Function3(
      paramArg1: CHType,
      paramArg2: CHType,
      paramArg3: CHType,
      arg1: CHType,
      arg2: CHType,
      arg3: CHType,
      output: CHType
  ) extends CHParametricFunction:
    val kind = "parametric3Function3"
    override val parameters = Seq(paramArg1, paramArg2, paramArg3)
    override val arguments = Seq(arg1, arg2, arg3)

  case class Parametric4Function0N(
      paramArg1: CHType,
      paramArg2: CHType,
      paramArg3: CHType,
      paramArg4: CHType,
      argN: CHType,
      output: CHType
  ) extends CHParametricFunctionNIO:
    val kind = "parametric4Function0N"
    override val parameters = Seq(paramArg1, paramArg2, paramArg3, paramArg4)
    override val arguments = Seq(argN)

  case class Parametric4Function1N(
      paramArg1: CHType,
      paramArg2: CHType,
      paramArg3: CHType,
      paramArg4: CHType,
      arg1: CHType,
      argN: CHType,
      output: CHType
  ) extends CHParametricFunctionNIO:
    val kind = "parametric4Function1N"
    override val parameters = Seq(paramArg1, paramArg2, paramArg3, paramArg4)
    override val arguments = Seq(arg1, argN)

  case class Parametric4Function2N(
      paramArg1: CHType,
      paramArg2: CHType,
      paramArg3: CHType,
      paramArg4: CHType,
      arg1: CHType,
      arg2: CHType,
      argN: CHType,
      output: CHType
  ) extends CHParametricFunctionNIO:
    val kind = "parametric4Function2N"
    override val parameters = Seq(paramArg1, paramArg2, paramArg3, paramArg4)
    override val arguments = Seq(arg1, arg2, argN)

  case class Parametric4Function1(
      paramArg1: CHType,
      paramArg2: CHType,
      paramArg3: CHType,
      paramArg4: CHType,
      arg1: CHType,
      output: CHType
  ) extends CHParametricFunction:
    val kind = "parametric4Function1"
    override val parameters = Seq(paramArg1, paramArg2, paramArg3, paramArg4)
    override val arguments = Seq(arg1)

  case class Parametric4Function2(
      paramArg1: CHType,
      paramArg2: CHType,
      paramArg3: CHType,
      paramArg4: CHType,
      arg1: CHType,
      arg2: CHType,
      output: CHType
  ) extends CHParametricFunction:
    val kind = "parametric4Function2"
    override val parameters = Seq(paramArg1, paramArg2, paramArg3, paramArg4)
    override val arguments = Seq(arg1, arg2)

  case class Parametric4Function3(
      paramArg1: CHType,
      paramArg2: CHType,
      paramArg3: CHType,
      paramArg4: CHType,
      arg1: CHType,
      arg2: CHType,
      arg3: CHType,
      output: CHType
  ) extends CHParametricFunction:
    val kind = "parametric4Function3"
    override val parameters = Seq(paramArg1, paramArg2, paramArg3, paramArg4)
    override val arguments = Seq(arg1, arg2, arg3)
