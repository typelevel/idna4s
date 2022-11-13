// scalafix:off

ThisBuild / scalafixScalaBinaryVersion := scalaBinaryVersion.value
ThisBuild / semanticdbEnabled          := true
ThisBuild / semanticdbVersion          := scalafixSemanticdb.revision

scalacOptions -= "-Ywarn-unused-import"

libraryDependencies ++= List(
  "org.scalameta"  % "semanticdb-scalac" % "4.6.0" cross CrossVersion.full,
  "org.scalameta" %% "scalameta"         % "4.6.0",
  "org.typelevel" %% "cats-core"         % "2.9.0",
  "org.typelevel" %% "kittens"           % "3.0.0"
)

libraryDependencies ++= List(
  "org.scalameta" %%% "munit-scalacheck" % "1.0.0-M6" % Test,
  "org.typelevel" %%% "cats-laws"        % "2.9.0"    % Test,
  "org.typelevel" %%% "discipline-munit" % "2.0.0-M3" % Test
)
