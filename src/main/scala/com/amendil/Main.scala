package com.amendil

import com.amendil.entities.{CHFunction, CHFunctionFuzzResult, CHFuzzableAbstractType, CHFuzzableType}
import com.amendil.fuzz._
import com.amendil.http.CHClient
import com.typesafe.scalalogging.Logger

import java.io.{File, PrintWriter}
import java.util.concurrent.Executors
import scala.collection.mutable.ArraySeq
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.concurrent.duration.Duration
import scala.util.Try

@main def app: Unit =
  val logger = Logger("Main")

  given ExecutionContext = ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(1))
  given CHClient = CHClient(Settings.ClickHouse.httpUrl)

  val runF =
    (for
      _ <- ensuringFuzzingValuesAreValid()

      chVersion <- getCHVersion()
      // functionNames <- getCHFunctions()
      functionNames <- Future.successful(unknownFunctions)
    yield {
      assume(Try { chVersion.toDouble }.isSuccess)

      val pw = PrintWriter(File(s"res/functions_v${chVersion}.txt.part"))
      val functionCount = functionNames.size

      val functionsFuzzResultsF: Future[Seq[CHFunctionFuzzResult]] =
        ConcurrencyUtils
          .executeInSequence(
            functionNames.zipWithIndex,
            (functionName: String, idx: Int) =>
              if idx % Math.max(functionCount / 20, 1) == 0 then
                logger.info(s"===============================================================")
                logger.info(s"${100 * idx / functionCount}%")
                logger.info(s"===============================================================")
              logger.info(functionName)

              if !functionName.toLowerCase.contains("json") then
                Fuzzer
                  .fuzz(functionName)
                  .recover { err =>
                    logger.error(s"Failed to fuzz function $functionName: ${err.getMessage()}")
                    CHFunctionFuzzResult(functionName)
                  }
                  .map { (fuzzResult: CHFunctionFuzzResult) =>
                    if !fuzzResult.atLeastOneSignatureFound then
                      logger.error(s"No signatures found for method $functionName")
                    pw.write(s"${CHFunction.fromCHFunctionFuzzResult(fuzzResult, "x64").asString()}\n")
                    fuzzResult
                  }
              else Future.successful(CHFunctionFuzzResult(name = functionName))
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

        logger.info(
          s"Rate of functions with a signature found: ${functionCount - functionsWithoutASignature.size}/$functionCount"
        )
        logger.info("Functions we were unable to determine any signature:")
        logger.info(functionsWithoutASignature.sorted.mkString("\"", "\", \"", "\""))
      }
    }).flatten

  Await.result(runF, Duration.Inf)

def ensuringFuzzingValuesAreValid()(using client: CHClient, ec: ExecutionContext): Future[Unit] =
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

def getCHFunctions()(using client: CHClient, ec: ExecutionContext): Future[Seq[String]] =
  client
    .execute("SELECT name, is_aggregate FROM system.functions")
    .map(_.data.map(_.head.asInstanceOf[String]).sorted)

def getCHVersion()(using client: CHClient, ec: ExecutionContext): Future[String] =
  client
    .execute("SELECT extract(version(), '^\\d+\\.\\d+') as version")
    .map(_.data.head.head.asInstanceOf[String])
