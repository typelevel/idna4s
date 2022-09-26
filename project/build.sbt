// ThisBuild / scalacOptions ++= List("-deprecation", "-encoding", "UTF-8", "-feature", "-unchecked", "-Yno-adapted-args", "-Ywarn-numeric-widen", "-Xlint:-unused","_", "-Xlint", "-Ywarn-dead-code", "-Ypartial-unification", "-Ybackend-parallelism", "16", "-language:_")

libraryDependencies ++= List(
  "org.scalameta" %% "scalameta" % "4.5.13",
  "org.typelevel" %% "cats-core" % "2.8.0"
)
