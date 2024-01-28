package com.amendil.bo

import com.amendil.CHType

final case class CHFunctionFuzzResult(
    name: String,
    functionNTypes: Seq[CHType] = Nil,
    isFunction0: Boolean = false,
    function1Types: Seq[CHType] = Nil,
    function2Types: Seq[(CHType, CHType)] = Nil,
    function3Types: Seq[(CHType, CHType, CHType)] = Nil
):
  override def toString(): String =
    s"$name(isFunctionN=${functionNTypes.nonEmpty}, isFunction0=$isFunction0, isFunction1=${function1Types.nonEmpty}, isFunction2=${function2Types.nonEmpty}, isFunction3=${function3Types.nonEmpty})"
