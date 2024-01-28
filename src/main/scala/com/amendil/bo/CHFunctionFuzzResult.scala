package com.amendil.bo

final case class CHFunctionFuzzResult(
    name: String,
    isFunction0: Boolean,
    isFunction1: Boolean
):
  override def toString(): String = s"$name($isFunction0, $isFunction1)"
