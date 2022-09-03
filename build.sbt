ThisBuild / tlBaseVersion := "0.0"

val Scala212 = "2.12.16"
val Scala213 = "2.13.8"
val Scala3   = "3.1.3"

val catsV      = "2.8.0"
val literallyV = "1.1.0"
val munitV     = "1.0.0-M6"

ThisBuild / crossScalaVersions         := Seq(Scala212, Scala213, Scala3)
ThisBuild / scalaVersion               := Scala213
ThisBuild / scalafixScalaBinaryVersion := (LocalRootProject / scalaBinaryVersion).value

ThisBuild / developers += tlGitHubDev("isomarcte", "David Strawn")
ThisBuild / licenses  := List(License.MIT)
ThisBuild / startYear := Some(2022)

// TODO remove me!
ThisBuild / tlFatalWarnings   := false
ThisBuild / tlCiScalafixCheck := false

// Utility

lazy val wildcardImport: SettingKey[Char] =
  settingKey[Char]("Character to use for wildcard imports.")
ThisBuild / wildcardImport := {
  if (tlIsScala3.value) {
    '*'
  } else {
    '_'
  }
}

// Projects

lazy val projectName: String = "idna4s"

lazy val root = tlCrossRootProject
  .aggregate(
    core
  )
  .settings(name := projectName)

lazy val core = crossProject(JVMPlatform, JSPlatform, NativePlatform)
  .crossType(CrossType.Pure)
  .in(file("core"))
  .settings(
    name := s"${projectName}-core",
    libraryDependencies ++= Seq(
      "org.typelevel" %%% "cats-core" % catsV
    ),
    libraryDependencies ++= {
      // Needed for macros
      if (tlIsScala3.value) {
        Nil
      } else {
        List(
          "org.typelevel" %%% "literally"     % literallyV,
          "org.scala-lang"  % "scala-reflect" % scalaVersion.value % Provided
        )
      }
    },
    libraryDependencies ++= Seq(
      "org.scalameta" %%% "munit-scalacheck" % munitV
    ).map(_ % Test),
    console / initialCommands := {
      List(
        "cats.",
        "cats.syntax.all.",
        "org.typelevel.idna4s.core.",
        "org.typelevel.idna4s.core.syntax.all.")
        .map(value => s"import ${value}${wildcardImport.value}")
        .mkString("\n")
    },
    consoleQuick / initialCommands := ""
  )
