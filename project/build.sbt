// scalafix:off

ThisBuild / scalacOptions ++= List(
  "-deprecation",
  "-encoding",
  "UTF-8",
  "-feature",
  "-unchecked",
  "-Yno-adapted-args",
  "-Ywarn-numeric-widen",
  "-Xlint:-unused,_",
  "-Xlint",
  "-Ywarn-dead-code",
  "-Ypartial-unification",
  "-Ybackend-parallelism",
  "16",
  "-language:_"
)

ThisBuild / scalafixDependencies ++= List(
  "com.github.liancheng" %% "organize-imports" % "0.6.0")
ThisBuild / scalafixScalaBinaryVersion := scalaBinaryVersion.value
ThisBuild / semanticdbEnabled          := true
ThisBuild / semanticdbVersion          := scalafixSemanticdb.revision

libraryDependencies ++= List(
  "org.scalameta"  % "semanticdb-scalac" % "4.6.0" cross CrossVersion.full,
  "org.scalameta" %% "scalameta"         % "4.6.0",
  "org.typelevel" %% "cats-core"         % "2.8.0"
)
