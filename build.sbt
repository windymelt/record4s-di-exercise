val scala3Version = "3.3.1"

lazy val root = project
  .in(file("."))
  .settings(
    name := "record4s-di-exercise",
    version := "0.1.0-SNAPSHOT",
    scalaVersion := scala3Version,
    libraryDependencies ++= Seq(
      "com.github.tarao" %% "record4s" % "0.11.0",
      "org.typelevel" %% "cats-core" % "2.10.0"
    ),
    libraryDependencies += "org.scalameta" %% "munit" % "0.7.29" % Test
  )
