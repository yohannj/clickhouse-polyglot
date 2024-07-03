package com.amendil.common.helper

import com.amendil.common.entities.`type`.*
import com.typesafe.scalalogging.StrictLogging
import fastparse.*
import fastparse.NoWhitespace.*
import fastparse.Parsed.{Failure, Success}

object CHTypeParser extends StrictLogging:

  def getByName(name: String): CHType =
    parse(name.replaceAll("\n\\s+", ""), root(using _)) match
      case Success(value, _) => value
      case Failure(label, idx, extra) =>
        logger.warn(s"Failed to parse type $name: label: $label, idx: $idx")
        throw IllegalArgumentException(s"Failed to parse type $name: label: $label, idx: $idx")

  private def root[$: P]: P[CHType] = P(Start ~ any ~ End)
  private def digits[$: P]: P[Int] = P(CharsWhileIn("0-9\\-").!).map(_.toInt)
  private def enumElement[$: P]: P[Unit] = (plainString ~ " = " ~ digits).map(_ => (): Unit)
  private def endOfConstant[$: P]: P[Unit] = P(!CharIn("A-Za-z0-9("))
  private def escapedString[$: P]: P[String] =
    "`" ~ CharsWhile(_ != '`').! ~ "`" // TODO an escaped String could have backticks in it
  private def plainString[$: P]: P[String] =
    "'" ~ CharsWhile(_ != '\'').! ~ "'" // TODO a plain String could have single quotes in it
  private def tupleElement[$: P]: P[CHType] =
    (escapedString ~ " " ~ any).map((_, chType) => chType) |
      (CharsWhileIn("A-Za-z0-9_") ~ " " ~ any) |
      any

  // ClickHouse types
  private def any[$: P]: P[CHType] = P(
    aggregateFunction | array | bitmap | datetime64 | datetimeTZ | datetime64TZ | decimal | enum16 | enum8 | fixedstring |
      json | lowcardinality | map | nullable | tuple | tupleN | _internal
  )
  private def aggregateFunction[$: P]: P[CHType] =
    P("AggregateFunction(" ~/ CharsWhile(_ != ',').! ~ ", " ~ any ~ ")").map { (fnName, innerType) =>
      fnName match
        case "groupBitmap" =>
          innerType match
            case CHFuzzableType.BooleanType => CHFuzzableType.BitmapBoolean
            case CHFuzzableType.Int8        => CHFuzzableType.BitmapInt8
            case CHFuzzableType.Int16       => CHFuzzableType.BitmapInt16
            case CHFuzzableType.Int32       => CHFuzzableType.BitmapInt32
            case CHFuzzableType.Int64       => CHFuzzableType.BitmapInt64
            case CHFuzzableType.UInt8       => CHFuzzableType.BitmapUInt8
            case CHFuzzableType.UInt16      => CHFuzzableType.BitmapUInt16
            case CHFuzzableType.UInt32      => CHFuzzableType.BitmapUInt32
            case CHFuzzableType.UInt64      => CHFuzzableType.BitmapUInt64
            case _                          => CHSpecialType.AggregateFunction(fnName, innerType)
        case _ => CHSpecialType.AggregateFunction(fnName, innerType)
    }
  private def array[$: P]: P[CHType] = P("Array(" ~/ any ~ ")").map(CHSpecialType.Array(_))
  private def bitmap[$: P]: P[CHType] = P("Bitmap(" ~/ any ~ ")").map(CHSpecialType.Bitmap(_))
  private def datetime64[$: P]: P[CHType] = P("DateTime64(" ~ digits ~ ")").map(_ => CHFuzzableType.DateTime64)
  private def datetimeTZ[$: P]: P[CHType] = P("DateTime(" ~/ plainString ~ ")").map(_ => CHFuzzableType.DateTime)
  private def datetime64TZ[$: P]: P[CHType] =
    P("DateTime64(" ~ digits ~ ", " ~ plainString ~ ")").map(_ => CHFuzzableType.DateTime64)
  private def decimal[$: P]: P[CHType] = P("Decimal(" ~/ digits ~ ", " ~/ digits ~ ")").map { (d1, _) =>
    if d1 <= 9 then CHFuzzableType.Decimal32
    else if d1 <= 18 then CHFuzzableType.Decimal64
    else if d1 <= 38 then CHFuzzableType.Decimal128
    else if d1 <= 76 then CHFuzzableType.Decimal256
    else throw new IllegalArgumentException(s"Unknown precision for Decimal: $d1, expected a precision up to 76.")
  }
  private def enum16[$: P]: P[CHType] =
    P("Enum16(" ~/ enumElement ~ ("," ~ " ".? ~ enumElement).rep ~ ")").map(_ => CHFuzzableType.Enum16)
  private def enum8[$: P]: P[CHType] =
    P("Enum8(" ~/ enumElement ~ ("," ~ " ".? ~ enumElement).rep ~ ")").map(_ => CHFuzzableType.Enum8)
  private def fixedstring[$: P]: P[CHType] = P("FixedString(" ~/ digits ~ ")").map(_ => CHFuzzableType.FixedString)
  private def json[$: P]: P[CHType] = P("Object('json')").map(_ => CHFuzzableType.Json)
  private def lowcardinality[$: P]: P[CHType] = P("LowCardinality(" ~/ any ~ ")").map(CHSpecialType.LowCardinality(_))
  private def map[$: P]: P[CHType] = P("Map(" ~/ any ~ "," ~ " ".? ~ any ~ ")").map(CHSpecialType.Map(_, _))
  private def nullable[$: P]: P[CHType] = P("Nullable(" ~/ any ~ ")").map(CHSpecialType.Nullable(_))
  private def tuple[$: P]: P[CHType] =
    P("Tuple(" ~/ tupleElement ~ ("," ~ " ".? ~ tupleElement).rep ~ ")").map((head, tail) =>
      CHSpecialType.Tuple(head +: tail)
    )
  private def tupleN[$: P]: P[CHType] = P("TupleN(" ~/ any ~ ")").map(CHSpecialType.TupleN(_))

  // Internal
  private val internalTypes = CHFuzzableType.values ++ CHAggregatedType.values ++ CHSpecialType.constantValues
  private def _internal[$: P]: P[CHType] = P(CharsWhileIn("A-Za-z0-9").!).collect {
    case s if internalTypes.exists(_.name == s) => internalTypes.find(_.name == s).get
    case s if Seq("T1", "T2", "T3", "T4", "T5", "T6", "T7").contains(s) =>
      CHSpecialType.GenericType(s, CHSpecialType.Nothing)
  }
