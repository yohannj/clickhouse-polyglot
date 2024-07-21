val javaVersion = "17"
val scala3Version = "3.4.2"

lazy val root = project
  .in(file("."))
  .settings(commonSettings)
  .aggregate(common, signature_fuzzer)

lazy val common = project
  .settings(
    name := "common",
    version := "0.1.0-SNAPSHOT",
    commonSettings,
    libraryDependencies ++= Seq(
      dependencies.config,
      dependencies.fastparse,
      dependencies.jacksonModuleScala,
      dependencies.logbackClassic,
      dependencies.scalaJava8Compact,
      dependencies.scalaLogging,
      dependencies.scalatestFreespec,
      dependencies.scalatestShouldMatchers
    )
  )

lazy val signature_fuzzer = project
  .settings(
    name := "signature_fuzzer",
    version := "0.1.0-SNAPSHOT",
    commonSettings,
    libraryDependencies ++= Seq(
      dependencies.config,
      dependencies.logbackClassic,
      dependencies.scalaLogging,
      dependencies.scalatestFreespec,
      dependencies.scalatestShouldMatchers
    )
  )
  .dependsOn(common)

lazy val dependencies = new {
  private val scalatestV = "3.2.19"

  // Logging
  val logbackClassic = "ch.qos.logback" % "logback-classic" % "1.5.6"
  val scalaLogging = "com.typesafe.scala-logging" %% "scala-logging" % "3.9.5"

  // Http
  val jacksonModuleScala = "com.fasterxml.jackson.module" %% "jackson-module-scala" % "2.17.2"

  // Misc
  val config = "com.typesafe" % "config" % "1.4.3"
  val fastparse = "com.lihaoyi" %% "fastparse" % "3.1.1"
  val scalaJava8Compact = "org.scala-lang.modules" %% "scala-java8-compat" % "1.0.2"

  // Test
  val scalatestFreespec = "org.scalatest" %% "scalatest-freespec" % scalatestV % Test
  val scalatestShouldMatchers = "org.scalatest" %% "scalatest-shouldmatchers" % scalatestV % Test
}

lazy val commonSettings = Seq(
  scalaVersion := scala3Version,
  javacOptions ++= Seq("-source", javaVersion, "-target", javaVersion),
  semanticdbEnabled := true,
  semanticdbVersion := scalafixSemanticdb.revision,
  scalacOptions ++= Seq(
    "-Xfatal-warnings",
    "-deprecation",
    "-feature",
    "-Wunused:imports"
  )
)
