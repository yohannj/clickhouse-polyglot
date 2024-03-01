package com.amendil.entities

sealed trait CHFunctionIO {
  def kind: String
  def input: Seq[CHType]
  def output: String

  def asString(): String =
    val inputStr = input.map(_.name).sorted.mkString(", ")

    s"$inputStr => $output".stripMargin
}

object CHFunctionIO {
  case class FunctionN(argN: CHType, output: String) extends CHFunctionIO {
    val kind = "functionN"
    val input = Seq(argN)
  }
  case class Function0(output: String) extends CHFunctionIO {
    val kind = "function0"
    val input = Nil
  }
  case class Function1(arg1: CHType, output: String) extends CHFunctionIO {
    val kind = "function1"
    val input = Seq(arg1)
  }

  case class Function2(arg1: CHType, arg2: CHType, output: String) extends CHFunctionIO {
    val kind = "function2"
    val input = Seq(arg1, arg2)
  }

  case class Function3(arg1: CHType, arg2: CHType, arg3: CHType, output: String) extends CHFunctionIO {
    val kind = "function3"
    val input = Seq(arg1, arg2, arg3)
  }

  case class Function4(arg1: CHType, arg2: CHType, arg3: CHType, arg4: CHType, output: String) extends CHFunctionIO {
    val kind = "function4"
    val input = Seq(arg1, arg2, arg3, arg4)
  }
}
