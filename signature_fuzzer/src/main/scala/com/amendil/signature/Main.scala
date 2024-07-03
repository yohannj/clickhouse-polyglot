package com.amendil.signature

import com.amendil.common.Settings as CommonSettings
import com.amendil.common.entities.`type`.*
import com.amendil.common.helper.{CHTypeMerger, ConcurrencyUtils}
import com.amendil.common.http.{CHClient, CHClientImpl}
import com.amendil.signature.entities.{CHFunctionFuzzResult, CHFuzzableAbstractType}
import com.amendil.signature.fuzz.*
import com.typesafe.scalalogging.Logger

import java.io.{File, PrintWriter}
import java.util.concurrent.Executors
import scala.collection.mutable.ArraySeq
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.concurrent.duration.Duration
import scala.util.{Failure, Success, Try}

@main def app: Unit =
  given logger: Logger = Logger("Main")

  given ExecutionContext = ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(1))
  given client: CHClient = CHClientImpl(Settings.ClickHouse.httpUrl)

  val runF =
    for
      _ <- ensuringFuzzingValuesAreValid()

      chVersion <- getCHVersion()
      _ = assume(Try { chVersion.toDouble }.isSuccess)

      _ <- createDictionaries(regexpTreeLocalPath = Settings.Fuzzer.regexpTreeYamlPath(chVersion))
      _ = describeCHTypesAndSave(chVersion)
      _ <- fuzzFunctionsAndSave(chVersion)
    yield ()

  Try(Await.result(runF, Duration.Inf)) match
    case Failure(exception) =>
      logger.error(exception.getMessage())
      exception.printStackTrace()
      sys.exit(1)
    case Success(_) =>
      sys.exit(0)

private def fuzzFunctionsAndSave(
    chVersion: String
)(using logger: Logger, client: CHClient, ec: ExecutionContext): Future[Unit] =
  // val functions <- getCHFunctions()
  val functions =
    unknownFunctions.map(
      CHFunctionFuzzResult(_, isAggregate = false, aliasTo = "")
    ) ++ unknownFunctionsWithAlias.map((name, alias) =>
      CHFunctionFuzzResult(name, isAggregate = false, aliasTo = alias)
    )

  val functionsToFuzz = functions.filter { fn =>
    Settings.Fuzzer.supportJson || !fn.name.toLowerCase().contains("json")
  }

  val pw = PrintWriter(File(s"res/functions_v${chVersion}.txt.part"))
  val functionCount = functionsToFuzz.size

  val functionsFuzzResultsF =
    ConcurrencyUtils
      .executeInSequence(
        functionsToFuzz.zipWithIndex, // .filter(_._1.name >= "caseWithoutExpression"),
        (function: CHFunctionFuzzResult, idx: Int) =>
          if idx % Math.max(functionCount / 20, 1) == 0 then
            logger.info(s"===============================================================")
            logger.info(s"${100 * idx / functionCount}%")
            logger.info(s"===============================================================")
          logger.info(function.name)

          Fuzzer
            .fuzz(function)
            .recover { err =>
              if err.getCause() == null then
                logger.error(s"Failed to fuzz function ${function.name}: ${err.getMessage()}")
              else logger.error(s"Failed to fuzz function ${function.name}: ${err.getCause().getMessage()}")
              function
            }
            .map { (fuzzResult: CHFunctionFuzzResult) =>
              if !fuzzResult.atLeastOneSignatureFound then
                logger.error(s"No signatures found for method ${function.name}")
              pw.write(s"${CHFunctionFuzzResult.toCHFunction(fuzzResult).asString()}\n")
              pw.flush()
              fuzzResult
            }
      )
      .recover(err =>
        pw.close()
        throw err
      )
      .map(res =>
        pw.close()
        res
      )

  functionsFuzzResultsF.map { (functionsFuzzResults: Seq[CHFunctionFuzzResult]) =>
    val functionsWithoutASignature: Seq[String] =
      functionsFuzzResults.filterNot(_.atLeastOneSignatureFound).map(_.name)

    logger.info(s"===============================================================")
    logger.info(s"100% - Fuzz finished")
    logger.info(s"===============================================================")
    logger.info(
      s"Rate of functions with a signature found: ${functionCount - functionsWithoutASignature.size}/$functionCount"
    )
    logger.info("Functions we were unable to determine any signature:")
    logger.info(functionsWithoutASignature.sorted.mkString("\"", "\", \"", "\""))
  }

private def describeCHTypesAndSave(chVersion: String): Unit =
  val pw = PrintWriter(File(s"res/types_v${chVersion}.txt.part"))

  CHAggregatedType.values.toSeq
    .sortBy(_.name)
    .map { t =>
      (
        t,
        CHTypeMerger.getSubtypes(t).collect { case t if t.isInstanceOf[CHFuzzableType] => t.name }.toSeq.sorted
      )
    }
    .foreach { case (aggregatedType, baseTypeNames: Seq[String]) =>
      pw.write(s"${aggregatedType.name}\n")
      baseTypeNames.foreach(t => pw.write(s"    $t\n"))
    }

  pw.close()

private def createDictionaries(regexpTreeLocalPath: String)(using CHClient, ExecutionContext): Future[Unit] =
  for
    _ <- createHierarchicalDictionary()
    _ <- createManyTypesDictionary()
    _ <- createRegexpTreeDictionary(regexpTreeLocalPath)
  yield (): Unit

private def createHierarchicalDictionary()(using client: CHClient, ec: ExecutionContext): Future[Unit] =
  for
    _ <- client.executeNoResultNoSettings(
      s"""|CREATE TABLE IF NOT EXISTS ${CommonSettings.Type.FuzzerDictionaryNames.hierarchyDictionaryName}_source_table
          |(
          |    childId UInt64,
          |    parentId UInt64
          |)
          |ENGINE = MergeTree()
          |ORDER BY childId""".stripMargin.replace("\n", " ")
    )

    _ <- client.executeNoResultNoSettings(
      s"""|CREATE DICTIONARY IF NOT EXISTS ${CommonSettings.Type.FuzzerDictionaryNames.hierarchyDictionaryName}
          |(
          |    childId UInt64,
          |    parentId UInt64 HIERARCHICAL
          |)
          |PRIMARY KEY childId
          |SOURCE(CLICKHOUSE(TABLE '${CommonSettings.Type.FuzzerDictionaryNames.hierarchyDictionaryName}_source_table'))
          |LAYOUT(FLAT())
          |LIFETIME(MIN 0 MAX 1000)
          |""".stripMargin.replace("\n", " ")
    )
  yield (): Unit

private def createManyTypesDictionary()(using client: CHClient, ec: ExecutionContext): Future[Unit] =
  val columns =
    """|(
       |    id UInt64,
       |    dateValue Date,
       |    dateTimeValue DateTime,
       |    float32Value Float32,
       |    float64Value Float64,
       |    int16Value Int16,
       |    int32Value Int32,
       |    int64Value Int64,
       |    int8Value Int8,
       |    iPv4Value IPv4,
       |    iPv6Value IPv6,
       |    stringValue String,
       |    uint16Value UInt16,
       |    uint32Value UInt32,
       |    uint64Value UInt64,
       |    uint8Value UInt8,
       |    uuidValue UUID
       |)""".stripMargin.replace("\n", " ")
  for
    _ <- client.executeNoResultNoSettings(
      s"""|CREATE TABLE IF NOT EXISTS ${CommonSettings.Type.FuzzerDictionaryNames.manyTypesDictionaryName}_source_table
          |$columns
          |ENGINE = MergeTree()
          |ORDER BY id""".stripMargin.replace("\n", " ")
    )

    _ <- client.executeNoResultNoSettings(
      s"""|CREATE DICTIONARY IF NOT EXISTS ${CommonSettings.Type.FuzzerDictionaryNames.manyTypesDictionaryName}
          |$columns
          |PRIMARY KEY id
          |SOURCE(CLICKHOUSE(TABLE '${CommonSettings.Type.FuzzerDictionaryNames.manyTypesDictionaryName}_source_table'))
          |LAYOUT(FLAT())
          |LIFETIME(MIN 0 MAX 1000)
          |""".stripMargin.replace("\n", " ")
    )
  yield (): Unit

private def createRegexpTreeDictionary(
    regexpTreeLocalPath: String
)(using client: CHClient, ec: ExecutionContext): Future[Unit] =
  client.executeNoResultNoSettings(
    s"""|CREATE DICTIONARY IF NOT EXISTS ${CommonSettings.Type.FuzzerDictionaryNames.regexpDictionaryName}
        |(
        |    regexp String,
        |    name String,
        |    version String
        |)
        |PRIMARY KEY regexp
        |SOURCE(YAMLRegExpTree(PATH '$regexpTreeLocalPath'))
        |LAYOUT(regexp_tree)
        |LIFETIME(MIN 0 MAX 1000)
        |""".stripMargin.replace("\n", " ")
  )

private def ensuringFuzzingValuesAreValid()(using client: CHClient, ec: ExecutionContext): Future[Unit] =
  Future
    .sequence(
      (CHFuzzableType.values.flatMap(_.fuzzingValues) ++ CHFuzzableAbstractType.values.flatMap(_.fuzzingValues))
        .map(v =>
          client
            .executeNoResult(s"SELECT toTypeName($v)")
            .map(_ => None)
            .recover(err => Some(s"$v: ${err.getMessage}"))
        )
    )
    .map { (results: ArraySeq[Option[String]]) =>
      val errors = results.flatten
      if errors.nonEmpty then throw Exception(s"Invalid fuzzing value founds.\n${errors.mkString("\n")}")
    }

private def getCHFunctions()(using client: CHClient, ec: ExecutionContext): Future[Seq[CHFunctionFuzzResult]] =
  client
    .execute("SELECT name, is_aggregate, alias_to FROM system.functions")
    .map(
      _.data
        .map { row =>
          CHFunctionFuzzResult(
            name = row(0).asInstanceOf[String],
            isAggregate = row(1).asInstanceOf[Int] == 1,
            aliasTo = row(2).asInstanceOf[String]
          )
        }
        .sortBy(_.name)
    )

private def getCHVersion()(using client: CHClient, ec: ExecutionContext): Future[String] =
  client
    .execute("SELECT extract(version(), '^\\d+\\.\\d+') as version")
    .map(_.data.head.head.asInstanceOf[String])
