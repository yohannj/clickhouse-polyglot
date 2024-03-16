package com.amendil.entities

sealed trait CHFunctionIO:
  def kind: String
  def parameters: Seq[CHType] = Nil
  def arguments: Seq[CHType] = Nil
  def hasInfiniteArgument: Boolean = false
  def output: String

  def asString(): String =
    val parametersStr = parameters.map(_.name).mkString(", ")
    val argumentsStr = arguments.map(_.name).mkString(", ") + (if hasInfiniteArgument then "..." else "")

    s"($parametersStr)($argumentsStr) => $output".stripMargin

sealed trait CHFunctionNIO extends CHFunctionIO:
  override def hasInfiniteArgument: Boolean = true

object CHFunctionIO:
  case class Function0N(argN: CHType, output: String) extends CHFunctionNIO:
    val kind = "function0N"
    override val arguments = Seq(argN)

  case class Function1N(arg1: CHType, argN: CHType, output: String) extends CHFunctionNIO:
    val kind = "function1N"
    override val arguments = Seq(arg1, argN)

  case class Function2N(arg1: CHType, arg2: CHType, argN: CHType, output: String) extends CHFunctionNIO:
    val kind = "function2N"
    override val arguments = Seq(arg1, arg2, argN)

  case class Function3N(arg1: CHType, arg2: CHType, arg3: CHType, argN: CHType, output: String) extends CHFunctionNIO:
    val kind = "function3N"
    override val arguments = Seq(arg1, arg2, arg3, argN)

  case class Function0(output: String) extends CHFunctionIO:
    val kind = "function0"

  case class Function1(arg1: CHType, output: String) extends CHFunctionIO:
    val kind = "function1"
    override val arguments = Seq(arg1)

  case class Function2(arg1: CHType, arg2: CHType, output: String) extends CHFunctionIO:
    val kind = "function2"
    override val arguments = Seq(arg1, arg2)

  case class Function3(arg1: CHType, arg2: CHType, arg3: CHType, output: String) extends CHFunctionIO:
    val kind = "function3"
    override val arguments = Seq(arg1, arg2, arg3)

  case class Function4(arg1: CHType, arg2: CHType, arg3: CHType, arg4: CHType, output: String) extends CHFunctionIO:
    val kind = "function4"
    override val arguments = Seq(arg1, arg2, arg3, arg4)

  case class LambdaFunction0N(lambdaArg: CHSpecialType.LambdaNType, argN: CHType, output: String) extends CHFunctionNIO:
    val kind = "lambdaFunction0N"
    override val arguments = Seq(lambdaArg, argN)

  case class LambdaFunction1N(lambdaArg: CHSpecialType.LambdaNType, arg1: CHType, argN: CHType, output: String)
      extends CHFunctionNIO:
    val kind = "lambdaFunction1N"
    override val arguments = Seq(lambdaArg, arg1, argN)

  case class Parametric1Function0N(paramArg1: CHType, argN: CHType, output: String) extends CHFunctionNIO:
    val kind = "parametric1Function0N"
    override val parameters = Seq(paramArg1)
    override val arguments = Seq(argN)

  case class Parametric1Function1N(paramArg1: CHType, arg1: CHType, argN: CHType, output: String) extends CHFunctionNIO:
    val kind = "parametric1Function1N"
    override val parameters = Seq(paramArg1)
    override val arguments = Seq(arg1, argN)

  case class Parametric1Function1(paramArg1: CHType, arg1: CHType, output: String) extends CHFunctionIO:
    val kind = "parametric1Function1"
    override val parameters = Seq(paramArg1)
    override val arguments = Seq(arg1)

  case class Parametric1Function2(paramArg1: CHType, arg1: CHType, arg2: CHType, output: String) extends CHFunctionIO:
    val kind = "parametric1Function2"
    override val parameters = Seq(paramArg1)
    override val arguments = Seq(arg1, arg2)

  case class Parametric2Function0N(paramArg1: CHType, paramArg2: CHType, argN: CHType, output: String)
      extends CHFunctionNIO:
    val kind = "parametric2Function0N"
    override val parameters = Seq(paramArg1, paramArg2)
    override val arguments = Seq(argN)

  case class Parametric2Function1N(
      paramArg1: CHType,
      paramArg2: CHType,
      arg1: CHType,
      argN: CHType,
      output: String
  ) extends CHFunctionNIO:
    val kind = "parametric2Function1N"
    override val parameters = Seq(paramArg1, paramArg2)
    override val arguments = Seq(arg1, argN)

  case class Parametric2Function1(paramArg1: CHType, paramArg2: CHType, arg1: CHType, output: String)
      extends CHFunctionIO:
    val kind = "parametric2Function1"
    override val parameters = Seq(paramArg1, paramArg2)
    override val arguments = Seq(arg1)

  case class Parametric2Function2(
      paramArg1: CHType,
      paramArg2: CHType,
      arg1: CHType,
      arg2: CHType,
      output: String
  ) extends CHFunctionIO:
    val kind = "parametric2Function2"
    override val parameters = Seq(paramArg1, paramArg2)
    override val arguments = Seq(arg1, arg2)

  case class Parametric3Function0N(
      paramArg1: CHType,
      paramArg2: CHType,
      paramArg3: CHType,
      argN: CHType,
      output: String
  ) extends CHFunctionNIO:
    val kind = "parametric3Function0N"
    override val parameters = Seq(paramArg1, paramArg2, paramArg3)
    override val arguments = Seq(argN)

  case class Parametric3Function1N(
      paramArg1: CHType,
      paramArg2: CHType,
      paramArg3: CHType,
      arg1: CHType,
      argN: CHType,
      output: String
  ) extends CHFunctionNIO:
    val kind = "parametric3Function1N"
    override val parameters = Seq(paramArg1, paramArg2, paramArg3)
    override val arguments = Seq(arg1, argN)

  case class Parametric3Function1(
      paramArg1: CHType,
      paramArg2: CHType,
      paramArg3: CHType,
      arg1: CHType,
      output: String
  ) extends CHFunctionIO:
    val kind = "parametric3Function1"
    override val parameters = Seq(paramArg1, paramArg2, paramArg3)
    override val arguments = Seq(arg1)

  case class Parametric3Function2(
      paramArg1: CHType,
      paramArg2: CHType,
      paramArg3: CHType,
      arg1: CHType,
      arg2: CHType,
      output: String
  ) extends CHFunctionIO:
    val kind = "parametric3Function2"
    override val parameters = Seq(paramArg1, paramArg2, paramArg3)
    override val arguments = Seq(arg1, arg2)

  case class Parametric4Function0N(
      paramArg1: CHType,
      paramArg2: CHType,
      paramArg3: CHType,
      paramArg4: CHType,
      argN: CHType,
      output: String
  ) extends CHFunctionNIO:
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
      output: String
  ) extends CHFunctionNIO:
    val kind = "parametric4Function1N"
    override val parameters = Seq(paramArg1, paramArg2, paramArg3, paramArg4)
    override val arguments = Seq(arg1, argN)

  case class Parametric4Function1(
      paramArg1: CHType,
      paramArg2: CHType,
      paramArg3: CHType,
      paramArg4: CHType,
      arg1: CHType,
      output: String
  ) extends CHFunctionIO:
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
      output: String
  ) extends CHFunctionIO:
    val kind = "parametric4Function2"
    override val parameters = Seq(paramArg1, paramArg2, paramArg3, paramArg4)
    override val arguments = Seq(arg1, arg2)

  case class Parametric5Function0N(
      paramArg1: CHType,
      paramArg2: CHType,
      paramArg3: CHType,
      paramArg4: CHType,
      paramArg5: CHType,
      argN: CHType,
      output: String
  ) extends CHFunctionNIO:
    val kind = "parametric5Function0N"
    override val parameters = Seq(paramArg1, paramArg2, paramArg3, paramArg4, paramArg5)
    override val arguments = Seq(argN)

  case class Parametric5Function1N(
      paramArg1: CHType,
      paramArg2: CHType,
      paramArg3: CHType,
      paramArg4: CHType,
      paramArg5: CHType,
      arg1: CHType,
      argN: CHType,
      output: String
  ) extends CHFunctionNIO:
    val kind = "parametric5Function1N"
    override val parameters = Seq(paramArg1, paramArg2, paramArg3, paramArg4, paramArg5)
    override val arguments = Seq(arg1, argN)

  case class Parametric5Function1(
      paramArg1: CHType,
      paramArg2: CHType,
      paramArg3: CHType,
      paramArg4: CHType,
      paramArg5: CHType,
      arg1: CHType,
      output: String
  ) extends CHFunctionIO:
    val kind = "parametric5Function1"
    override val parameters = Seq(paramArg1, paramArg2, paramArg3, paramArg4, paramArg5)
    override val arguments = Seq(arg1)

  case class Parametric5Function2(
      paramArg1: CHType,
      paramArg2: CHType,
      paramArg3: CHType,
      paramArg4: CHType,
      paramArg5: CHType,
      arg1: CHType,
      arg2: CHType,
      output: String
  ) extends CHFunctionIO:
    val kind = "parametric5Function2"
    override val parameters = Seq(paramArg1, paramArg2, paramArg3, paramArg4, paramArg5)
    override val arguments = Seq(arg1, arg2)
