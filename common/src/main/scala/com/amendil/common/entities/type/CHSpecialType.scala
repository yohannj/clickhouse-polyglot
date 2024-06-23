package com.amendil.common.entities.`type`

trait InnerType:
  def innerType: CHType

/**
  * CHSpecialType regroups three kind of types:
  * 1/ Types that exists in ClickHouse and that takes other types in their arguments
  * 2/ Weird types like AggregateFunction and Lambda.
  * 3/ Function parameters/arguments that only few specific Strings as input
  */
enum CHSpecialType(val name: String) extends CHType:
  case AggregateFunction(fnName: String, innerType: CHType)
      extends CHSpecialType(s"AggregateFunction($fnName, ${innerType.name})")
  case Array(innerType: CHType) extends CHSpecialType(s"Array(${innerType.name})") with InnerType
  case Bitmap(innerType: CHType) extends CHSpecialType(s"Bitmap(${innerType.name})") with InnerType
  case GenericType(typeName: String, superType: CHType) extends CHSpecialType(typeName)
  case LambdaType(outputType: CHType) extends CHSpecialType(s"Lambda(${outputType.name})")
  case LowCardinality(innerType: CHType) extends CHSpecialType(s"LowCardinality(${innerType.name})") with InnerType
  case Map(keyType: CHType, valueType: CHType) extends CHSpecialType(s"Map(${keyType.name}, ${valueType.name})")
  case Nullable(innerType: CHType) extends CHSpecialType(s"Nullable(${innerType.name})") with InnerType
  case Tuple(innerTypes: Seq[CHType]) extends CHSpecialType(s"Tuple(${innerTypes.map(_.name).mkString(", ")})")
  case TupleN(innerType: CHType) extends CHSpecialType(s"TupleN(${innerType.name})") with InnerType

  // This could be seen as a CHAggregatedType, but we want to allow it only for Catboost functions
  case CatboostParameter extends CHSpecialType("CatboostParameter") // UIntX, IntX, FloatX, Date, Date32, DateTime

  case Nothing extends CHSpecialType("Nothing")

  case SequenceBaseFirstMatch extends CHSpecialType("SequenceBaseFirstMatch") // "'first_match'"
  case SequenceBaseHead extends CHSpecialType("SequenceBaseHead") // "'head'"
  case SequenceBaseLastMatch extends CHSpecialType("SequenceBaseLastMatch") // "'last_match'"
  case SequenceBaseTail extends CHSpecialType("SequenceBaseTail") // "'tail'"
  case SequenceDirectionForward extends CHSpecialType("SequenceDirectionForward") // "'forward'"
  case SequenceDirectionBackward extends CHSpecialType("SequenceDirectionBackward") // "'backward'"

  case UnknownType extends CHSpecialType("UnknownType") // Any kind of Integer, signed or not.

object CHSpecialType:
  val constantValues: Set[CHSpecialType] = Set(
    CHSpecialType.CatboostParameter,
    CHSpecialType.Nothing,
    CHSpecialType.SequenceBaseFirstMatch,
    CHSpecialType.SequenceBaseHead,
    CHSpecialType.SequenceBaseLastMatch,
    CHSpecialType.SequenceBaseTail,
    CHSpecialType.SequenceDirectionForward,
    CHSpecialType.SequenceDirectionBackward,
    CHSpecialType.UnknownType
  )
