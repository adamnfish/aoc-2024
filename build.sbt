ThisBuild / organization := "com.adamnfish"
ThisBuild / scalaVersion := "3.5.2"

lazy val root = (project in file(".")).settings(
  name := "aoc-2024",
  libraryDependencies ++= Seq(
    "org.typelevel" %% "cats-core" % "2.12.0",
    "org.typelevel" %% "cats-effect" % "3.5.6",
    "org.typelevel" %% "cats-effect-kernel" % "3.5.6",
    "org.typelevel" %% "cats-effect-std" % "3.5.6",
    "co.fs2" %% "fs2-core" % "3.11.0",
    "co.fs2" %% "fs2-io" % "3.11.0",
    "com.lihaoyi" %% "fastparse" % "3.1.1",
    "com.softwaremill.sttp.client3" %% "cats" % "3.10.1",
    "org.scalatest" %% "scalatest" % "3.2.19" % Test,
    "org.scalacheck" %% "scalacheck" % "1.18.1" % Test,
    "org.scalatestplus" %% "scalacheck-1-15" % "3.2.11.0" % Test,
  ),
  Compile / run / fork := true,
)
