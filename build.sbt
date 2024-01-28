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
        "-feature"
      ),
    libraryDependencies ++= Seq(
      "com.typesafe.scala-logging" %% "scala-logging" % "3.9.5",
      "org.scala-lang.modules" %% "scala-java8-compat" % "1.0.2",

      // Json
      "com.fasterxml.jackson.module" %% "jackson-module-scala" % "2.16.1"
    )
  )

ThisBuild / scalafixDependencies += "com.github.liancheng" %% "organize-imports" % "0.6.0"
