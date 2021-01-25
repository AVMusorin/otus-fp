import sbt._

object Dependencies {

  object V {
    // Scala
    val decline          = "1.3.0"
    val circe            = "0.13.0"
    val http4s           = "0.21.13"
    val fs2              = "2.5.0"
    // Scala (test only)
    val specs2           = "4.10.5"
    val scalaCheck       = "1.15.1"
  }

  // Scala
  val all = List(
    "com.monovore"               %% "decline"              % V.decline,
    "io.circe"                   %% "circe-core"           % V.circe,
    "io.circe"                   %% "circe-parser"         % V.circe,
    "io.circe"                   %% "circe-generic"        % V.circe,
    "org.http4s"                 %% "http4s-blaze-server"  % V.http4s,
    "org.http4s"                 %% "http4s-circe"         % V.http4s,
    "org.http4s"                 %% "http4s-dsl"           % V.http4s,
    "co.fs2"                     %% "fs2-io"               % V.fs2,

    "org.specs2"                 %% "specs2-core"          % V.specs2         % Test,
    "org.scalacheck"             %% "scalacheck"           % V.scalaCheck     % Test
  )
}
