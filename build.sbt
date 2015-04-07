name := "ScalaSpreadsheet"

version := "1.0"

scalaVersion := "2.11.6"

libraryDependencies := {
  CrossVersion.partialVersion(scalaVersion.value) match {
    // if scala 2.11+ is used, add dependency on scala-xml module
    case Some((2, scalaMajor)) if scalaMajor >= 11 =>
      libraryDependencies.value ++ Seq(
        "org.scala-lang.modules" %% "scala-xml" % "1.0.3",
        "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.3",
        "org.scala-lang.modules" %% "scala-swing" % "1.0.1",
        "org.specs2" %% "specs2-core" % "3.3.1" % "test"
      )
    case _ =>
      // or just libraryDependencies.value if you don't depend on scala-swing
      libraryDependencies.value :+ "org.scala-lang" % "scala-swing" % scalaVersion.value
  }
}