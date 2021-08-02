lazy val root = project
  .in(file("."))
  .settings(
    name := "reactive-web",
    version := "0.1.0",
    scalaVersion := "2.13.6",
    libraryDependencies ++= Seq (
      "org.typelevel" %% "cats-core" % "2.3.0",
      "org.typelevel" %% "cats-effect" % "3.2.0"
    )
  )