name := "macros"

scalacOptions in (Compile, console) --= Seq(
  "-Ywarn-unused:imports",
  "-Xfatal-warnings"
)

libraryDependencies ++= Seq(
  scalaOrganization.value % "scala-reflect" % scalaVersion.value,
  scalaOrganization.value % "scala-compiler" % scalaVersion.value
)
