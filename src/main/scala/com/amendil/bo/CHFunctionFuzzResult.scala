package com.amendil.bo

import com.amendil.CHTypes

final case class CHFunctionFuzzResult(
    name: String,
    functionNTypes: Seq[CHTypes[_]] = Nil,
    isFunction0: Boolean = false,
    function1Types: Seq[CHTypes[_]] = Nil,
    function2Types: Seq[(CHTypes[_], CHTypes[_])] = Nil,
    function3Types: Seq[(CHTypes[_], CHTypes[_], CHTypes[_])] = Nil
):
  override def toString(): String =
    s"$name(isFunctionN=${functionNTypes.nonEmpty}, isFunction0=$isFunction0, isFunction1=${function1Types.nonEmpty}, isFunction2=${function2Types.nonEmpty}, isFunction3=${function3Types.nonEmpty})"
