package com.amendil.entities

sealed trait CHFunctionIO:
  def kind: String
  def input: Seq[CHType]
  def output: String

  def asString(): String =
    val inputStr = input.map(_.name).mkString(", ")

    s"$inputStr => $output".stripMargin

object CHFunctionIO:
  case class Function0N(argN: CHType, output: String) extends CHFunctionIO:
    val kind = "function0N"
    val input = Seq(argN)

  case class Function1N(arg1: CHType, argN: CHType, output: String) extends CHFunctionIO:
    val kind = "function1N"
    val input = Seq(arg1, argN)

  case class Function2N(arg1: CHType, arg2: CHType, argN: CHType, output: String) extends CHFunctionIO:
    val kind = "function2N"
    val input = Seq(arg1, arg2, argN)

  case class Function3N(arg1: CHType, arg2: CHType, arg3: CHType, argN: CHType, output: String) extends CHFunctionIO:
    val kind = "function3N"
    val input = Seq(arg1, arg2, arg3, argN)

  case class Function0(output: String) extends CHFunctionIO:
    val kind = "function0"
    val input = Nil

  case class Function1(arg1: CHType, output: String) extends CHFunctionIO:
    val kind = "function1"
    val input = Seq(arg1)

  case class Function2(arg1: CHType, arg2: CHType, output: String) extends CHFunctionIO:
    val kind = "function2"
    val input = Seq(arg1, arg2)

  case class Function3(arg1: CHType, arg2: CHType, arg3: CHType, output: String) extends CHFunctionIO:
    val kind = "function3"
    val input = Seq(arg1, arg2, arg3)

  case class Function4(arg1: CHType, arg2: CHType, arg3: CHType, arg4: CHType, output: String) extends CHFunctionIO:
    val kind = "function4"
    val input = Seq(arg1, arg2, arg3, arg4)

  case class LambdaFunction0N(lambdaArg: CHSpecialType.LambdaNType, argN: CHType, output: String) extends CHFunctionIO:
    val kind = "lambdaFunction0N"
    val input = Seq(lambdaArg, argN)

  case class LambdaFunction1N(lambdaArg: CHSpecialType.LambdaNType, arg1: CHType, argN: CHType, output: String)
      extends CHFunctionIO:
    val kind = "lambdaFunction1N"
    val input = Seq(lambdaArg, arg1, argN)

  case class Parametric1Function0N(paramArg1: CHType, argN: CHType, output: String) extends CHFunctionIO:
    val kind = "parametric1Function0N"
    val input = Seq(paramArg1, argN)

  case class Parametric1Function1N(paramArg1: CHType, arg1: CHType, argN: CHType, output: String) extends CHFunctionIO:
    val kind = "parametric1Function1N"
    val input = Seq(paramArg1, arg1, argN)

  case class Parametric1Function1(paramArg1: CHType, arg1: CHType, output: String) extends CHFunctionIO:
    val kind = "parametric1Function1"
    val input = Seq(paramArg1, arg1)

  case class Parametric1Function2(paramArg1: CHType, arg1: CHType, arg2: CHType, output: String) extends CHFunctionIO:
    val kind = "parametric1Function2"
    val input = Seq(paramArg1, arg1, arg2)

  case class Parametric2Function0N(paramArg1: CHType, paramArg2: CHType, argN: CHType, output: String)
      extends CHFunctionIO:
    val kind = "parametric2Function0N"
    val input = Seq(paramArg1, paramArg2, argN)

  case class Parametric2Function1N(
      paramArg1: CHType,
      paramArg2: CHType,
      arg1: CHType,
      argN: CHType,
      output: String
  ) extends CHFunctionIO:
    val kind = "parametric2Function1N"
    val input = Seq(paramArg1, paramArg2, arg1, argN)

  case class Parametric2Function1(paramArg1: CHType, paramArg2: CHType, arg1: CHType, output: String)
      extends CHFunctionIO:
    val kind = "parametric2Function1"
    val input = Seq(paramArg1, paramArg2, arg1)

  case class Parametric2Function2(
      paramArg1: CHType,
      paramArg2: CHType,
      arg1: CHType,
      arg2: CHType,
      output: String
  ) extends CHFunctionIO:
    val kind = "parametric2Function2"
    val input = Seq(paramArg1, paramArg2, arg1, arg2)

  case class Parametric3Function0N(
      paramArg1: CHType,
      paramArg2: CHType,
      paramArg3: CHType,
      argN: CHType,
      output: String
  ) extends CHFunctionIO:
    val kind = "parametric3Function0N"
    val input = Seq(paramArg1, paramArg2, paramArg3, argN)

  case class Parametric3Function1N(
      paramArg1: CHType,
      paramArg2: CHType,
      paramArg3: CHType,
      arg1: CHType,
      argN: CHType,
      output: String
  ) extends CHFunctionIO:
    val kind = "parametric3Function1N"
    val input = Seq(paramArg1, paramArg2, paramArg3, arg1, argN)

  case class Parametric3Function1(
      paramArg1: CHType,
      paramArg2: CHType,
      paramArg3: CHType,
      arg1: CHType,
      output: String
  ) extends CHFunctionIO:
    val kind = "parametric3Function1"
    val input = Seq(paramArg1, paramArg2, paramArg3, arg1)

  case class Parametric3Function2(
      paramArg1: CHType,
      paramArg2: CHType,
      paramArg3: CHType,
      arg1: CHType,
      arg2: CHType,
      output: String
  ) extends CHFunctionIO:
    val kind = "parametric3Function2"
    val input = Seq(paramArg1, paramArg2, paramArg3, arg1, arg2)
