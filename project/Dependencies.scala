import sbt._

object Dependencies {
  lazy val scalaTest = Seq("org.scalatest" %% "scalatest" % "3.0.5" % Test)

  val catsVersion = "1.3.1"
  val circeVersion = "0.9.3"
  val scrimageVersion = "2.1.8"
  val silencerVersion = "1.2"

  lazy val circe = Seq(
    "io.circe" %% "circe-core",
    "io.circe" %% "circe-generic",
    "io.circe" %% "circe-parser"
  ).map(_ % circeVersion)

  lazy val cats = Seq(
    "org.typelevel" %% "cats-core" % catsVersion,
    "org.typelevel" %% "mouse" % "0.18"
  )

  lazy val shapeless = Seq(
    "com.chuusai" %% "shapeless" % "2.3.3"
  )

  lazy val simulacrum = Seq("com.github.mpilquist" %% "simulacrum" % "0.13.0")

  lazy val scrimage = Seq(
    "com.sksamuel.scrimage" %% "scrimage-core",
    "com.sksamuel.scrimage" %% "scrimage-io-extra",
    "com.sksamuel.scrimage" %% "scrimage-filters"
  ).map(_ % scrimageVersion)

  lazy val logging = Seq(
    "ch.qos.logback" % "logback-classic" % "1.2.3",
    "com.typesafe.scala-logging" %% "scala-logging" % "3.9.0"
  )

  lazy val tagless = Seq(
    "org.typelevel" %% "cats-tagless-macros" % "0.1.0"
  )

  lazy val silencer = Seq(
    compilerPlugin("com.github.ghik" %% "silencer-plugin" % silencerVersion),
    "com.github.ghik" %% "silencer-lib" % silencerVersion % Provided
  )

  lazy val all = cats ++ circe ++ shapeless ++ simulacrum ++ scrimage ++ scalaTest ++ logging ++ tagless ++ silencer

}
