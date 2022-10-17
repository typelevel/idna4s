// scalafix:off

ThisBuild / scalafixScalaBinaryVersion := scalaBinaryVersion.value
ThisBuild / semanticdbEnabled          := true
ThisBuild / semanticdbVersion          := scalafixSemanticdb.revision

scalacOptions -= "-Ywarn-unused-import"

libraryDependencies ++= List(
  "org.scalameta"  % "semanticdb-scalac" % "4.6.0" cross CrossVersion.full,
  "org.scalameta" %% "scalameta"         % "4.6.0",
  "org.typelevel" %% "cats-core"         % "2.8.0"
)
