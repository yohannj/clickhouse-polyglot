package com.amendil.common.entities.`type`

enum CHSpecialType(val name: String) extends CHType:
  case AggregateFunction(fnName: String, innerType: CHType)
      extends CHSpecialType(s"AggregateFunction($fnName, ${innerType.name})")
  case Array(innerType: CHType) extends CHSpecialType(s"Array(${innerType.name})")
  case GenericType(typeName: String) extends CHSpecialType(typeName)
  case LambdaType(outputType: CHType) extends CHSpecialType(s"Lambda(${outputType.name})")
  case LowCardinality(innerType: CHType) extends CHSpecialType(s"LowCardinality(${innerType.name})")
  case Map(keyType: CHType, valueType: CHType) extends CHSpecialType(s"Map(${keyType.name}, ${valueType.name})")
  case Nullable(innerType: CHType) extends CHSpecialType(s"Nullable(${innerType.name})")
  case Tuple(innerTypes: Seq[CHType]) extends CHSpecialType(s"Tuple(${innerTypes.map(_.name).mkString(", ")})")
  case TupleN(innerType: CHType) extends CHSpecialType(s"TupleN(${innerType.name})")

  case CatboostParameter
      extends CHSpecialType("CatboostParameter") // UIntX, IntX, Float32, Float64, Date, Date32, DateTime

  case Nothing extends CHSpecialType("Nothing")

  case SequenceBaseFirstMatch extends CHSpecialType("SequenceBaseFirstMatch") // "'first_match'"
  case SequenceBaseHead extends CHSpecialType("SequenceBaseHead") // "'head'"
  case SequenceBaseLastMatch extends CHSpecialType("SequenceBaseLastMatch") // "'last_match'"
  case SequenceBaseTail extends CHSpecialType("SequenceBaseTail") // "'tail'"
  case SequenceDirectionForward extends CHSpecialType("SequenceDirectionForward") // "'forward'"
  case SequenceDirectionBackward extends CHSpecialType("SequenceDirectionBackward") // "'backward'"
