package com.amendil.entities

sealed trait CHFunctionIO {
  def kind: String
  def input: Seq[CHInputType]
  def output: String

  def asString(): String =
    val inputStr = input.map(_.name).sorted.mkString(", ")

    s"$inputStr => $output".stripMargin
}

object CHFunctionIO {
  case class Function0N(argN: CHInputType, output: String) extends CHFunctionIO {
    val kind = "function0N"
    val input = Seq(argN)
  }
  case class Function1N(arg1: CHInputType, argN: CHInputType, output: String) extends CHFunctionIO {
    val kind = "function1N"
    val input = Seq(arg1, argN)
  }
  case class Function2N(arg1: CHInputType, arg2: CHInputType, argN: CHInputType, output: String) extends CHFunctionIO {
    val kind = "function2N"
    val input = Seq(arg1, arg2, argN)
  }
  case class Function3N(arg1: CHInputType, arg2: CHInputType, arg3: CHInputType, argN: CHInputType, output: String)
      extends CHFunctionIO {
    val kind = "function3N"
    val input = Seq(arg1, arg2, arg3, argN)
  }
  case class Function0(output: String) extends CHFunctionIO {
    val kind = "function0"
    val input = Nil
  }
  case class Function1(arg1: CHInputType, output: String) extends CHFunctionIO {
    val kind = "function1"
    val input = Seq(arg1)
  }

  case class Function2(arg1: CHInputType, arg2: CHInputType, output: String) extends CHFunctionIO {
    val kind = "function2"
    val input = Seq(arg1, arg2)
  }

  case class Function3(arg1: CHInputType, arg2: CHInputType, arg3: CHInputType, output: String) extends CHFunctionIO {
    val kind = "function3"
    val input = Seq(arg1, arg2, arg3)
  }

  case class Function4(arg1: CHInputType, arg2: CHInputType, arg3: CHInputType, arg4: CHInputType, output: String)
      extends CHFunctionIO {
    val kind = "function4"
    val input = Seq(arg1, arg2, arg3, arg4)
  }
}
