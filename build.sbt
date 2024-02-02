val scala3Version = "3.3.1"

val pekkoVersion = "1.0.2"

lazy val root = project
  .in(file("."))
  .settings(
    name := "fuzz",
    version := "0.1.0-SNAPSHOT",
    scalaVersion := scala3Version,
    semanticdbEnabled := true,
    semanticdbVersion := scalafixSemanticdb.revision,
    scalacOptions ++=
      Seq(
        "-Xfatal-warnings",
        "-deprecation",
        "-feature",
        "-Wunused:imports"
      ),
    libraryDependencies ++= Seq(
      // Logging
      "com.typesafe.scala-logging" %% "scala-logging" % "3.9.5",
      "ch.qos.logback" % "logback-classic" % "1.3.5",

      // Http
      "com.fasterxml.jackson.module" %% "jackson-module-scala" % "2.16.1",
      "com.softwaremill.retry" %% "retry" % "0.3.6",

      // Misc
      "org.scala-lang.modules" %% "scala-java8-compat" % "1.0.2",

      // Test
      "org.scalatest" %% "scalatest-freespec" % "3.2.17" % Test,
      "org.scalatest" %% "scalatest-shouldmatchers" % "3.2.17" % Test
    )
  )
