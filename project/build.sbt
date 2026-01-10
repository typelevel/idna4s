// scalafix:off

ThisBuild / scalafixScalaBinaryVersion := scalaBinaryVersion.value
ThisBuild / semanticdbEnabled          := true
ThisBuild / semanticdbVersion          := scalafixSemanticdb.revision

scalacOptions -= "-Ywarn-unused-import"

// Extracted from sbt-typelevel's configuration. We don't want all of
// sbt-typelevel for the meta project build, but headers would be nice.
licenses += "Apache-2.0" -> url("https://www.apache.org/licenses/LICENSE-2.0.txt")
organizationName         := "Typelevel"
startYear                := Some(2022)

libraryDependencies ++= List(
  "org.scalameta"  % "semanticdb-scalac" % "4.13.10" cross CrossVersion.full,
  "org.scalameta" %% "scalameta"         % "4.13.10",
  "org.typelevel" %% "cats-core"         % "2.13.0",
  "org.typelevel" %% "kittens"           % "3.5.0"
)

libraryDependencies ++= List(
  "org.scalameta" %%% "munit-scalacheck" % "1.1.0"  % Test,
  "org.typelevel" %%% "cats-laws"        % "2.13.0" % Test,
  "org.typelevel" %%% "discipline-munit" % "2.0.0"  % Test
)
