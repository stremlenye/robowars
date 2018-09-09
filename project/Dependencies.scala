import sbt._

object Dependencies {
  lazy val scalaTest = Seq("org.scalatest" %% "scalatest" % "3.0.5" % Test)

  val catsVersion = "1.3.1"
  val circeVersion = "0.9.3"
  val scrimageVersion = "2.1.8"

  lazy val circe = Seq(
    "io.circe" %% "circe-core",
    "io.circe" %% "circe-generic",
    "io.circe" %% "circe-parser"
  ).map(_ % circeVersion)

  lazy val cats = Seq("org.typelevel" %% "cats-core" % catsVersion)

  lazy val shapeless = Seq(
    "com.chuusai" %% "shapeless" % "2.3.3"
  )

  lazy val simulacrum = Seq("com.github.mpilquist" %% "simulacrum" % "0.13.0")

  lazy val scrimage = Seq("com.sksamuel.scrimage" %% "scrimage-core",
    "com.sksamuel.scrimage" %% "scrimage-io-extra",
    "com.sksamuel.scrimage" %% "scrimage-filters").map(_ % scrimageVersion)

  lazy val all = cats ++ circe ++ shapeless ++ simulacrum ++ scrimage ++ scalaTest

}
