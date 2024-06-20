package com.amendil.common.entities.`type`

import com.amendil.common.helper.CHTypeParser
import com.typesafe.scalalogging.StrictLogging

trait CHType:
  def name: String

object CHType extends StrictLogging:

  def getGenericTypes(types: Seq[CHType]): Set[CHSpecialType.GenericType] =
    types.toSet.flatMap(getGenericTypes(_))

  def getGenericTypes(t: CHType): Set[CHSpecialType.GenericType] =
    t match
      case t: CHSpecialType.GenericType    => Set(t)
      case arr: CHSpecialType.Array        => getGenericTypes(arr.innerType)
      case b: CHSpecialType.Bitmap         => getGenericTypes(b.innerType)
      case l: CHSpecialType.LowCardinality => getGenericTypes(l.innerType)
      case m: CHSpecialType.Map            => getGenericTypes(m.keyType) ++ getGenericTypes(m.valueType)
      case n: CHSpecialType.Nullable       => getGenericTypes(n.innerType)
      case t: CHSpecialType.Tuple          => t.innerTypes.toSet.flatMap(getGenericTypes)
      case t: CHSpecialType.TupleN         => getGenericTypes(t.innerType)
      case _                               => Set.empty

  /**
    * Tries to rewrite the given type using CHSpecialType as much as possible
    * E.g. CHFuzzable.ArrayUInt8 will become CHSpecialType.Array(CHFuzzable.UInt8)
    *
    * This method guarantees that for different CHType having the same name
    * it will return a similar CHType.
    */
  def normalize(t: CHType): CHType =
    t match
      case CHSpecialType.AggregateFunction(fnName, innerType) =>
        CHSpecialType.AggregateFunction(fnName, normalize(innerType))
      case CHSpecialType.Array(innerType)                 => CHSpecialType.Array(normalize(innerType))
      case CHSpecialType.Bitmap(innerType)                => CHSpecialType.Bitmap(normalize(innerType))
      case CHSpecialType.GenericType(typeName, superType) => CHSpecialType.GenericType(typeName, normalize(superType))
      case CHSpecialType.LambdaType(outputType)           => CHSpecialType.LambdaType(normalize(outputType))
      case CHSpecialType.LowCardinality(innerType)        => CHSpecialType.LowCardinality(normalize(innerType))
      case CHSpecialType.Map(keyType, valueType)          => CHSpecialType.Map(normalize(keyType), normalize(valueType))
      case CHSpecialType.Nullable(innerType)              => CHSpecialType.Nullable(normalize(innerType))
      case CHSpecialType.Tuple(innerTypes)                => CHSpecialType.Tuple(innerTypes.map(normalize))
      case CHSpecialType.TupleN(innerType)                => CHSpecialType.TupleN(normalize(innerType))
      case _                                              => CHTypeParser.getByName(t.name)
