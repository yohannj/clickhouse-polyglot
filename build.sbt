val javaVersion = "17"
val scala3Version = "3.3.1"

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
      dependencies.scalaLogging,
      dependencies.logbackClassic,
      dependencies.config,
      dependencies.jacksonModuleScala,
      dependencies.scalaJava8Compact
    )
  )

lazy val signature_fuzzer = project
  .settings(
    name := "signature_fuzzer",
    version := "0.1.0-SNAPSHOT",
    commonSettings,
    libraryDependencies ++= Seq(
      dependencies.scalaLogging,
      dependencies.logbackClassic,
      dependencies.config,
      dependencies.scalatestFreespec,
      dependencies.scalatestShouldMatchers
    )
  )
  .dependsOn(common)

lazy val dependencies = new {
  private val scalatestV = "3.2.18"

  // Logging
  val scalaLogging = "com.typesafe.scala-logging" %% "scala-logging" % "3.9.5"
  val logbackClassic = "ch.qos.logback" % "logback-classic" % "1.5.3"

  // Http
  val jacksonModuleScala = "com.fasterxml.jackson.module" %% "jackson-module-scala" % "2.17.0"

  // Misc
  val scalaJava8Compact = "org.scala-lang.modules" %% "scala-java8-compat" % "1.0.2"
  val config = "com.typesafe" % "config" % "1.4.3"

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
    "-Wunused:imports",
    "-new-syntax"
  )
)
