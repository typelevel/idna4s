import org.typelevel.idna4s.build._

ThisBuild / tlBaseVersion := "0.1"

val UnicodeVersion: String = "17.0.0"

val Scala212                    = "2.12.21"
val Scala213                    = "2.13.18"
val Scala3                      = "3.3.7"
def DefaultScalaVersion: String = Scala213

val catsCollectionsV = "0.9.10"
val catsV            = "2.13.0"
val disciplineMunitV = "2.0.0"
val icu4jV           = "78.2"
val literallyV       = "1.2.0"
val munitV           = "1.2.1"
val munitScalacheckV = "1.2.0"
val scalacheckV      = "1.19.0"

ThisBuild / crossScalaVersions := Seq(Scala212, Scala213, Scala3)
ThisBuild / scalaVersion       := Scala213
ThisBuild / developers += tlGitHubDev("isomarcte", "David Strawn")
ThisBuild / licenses  := List(License.MIT)
ThisBuild / startYear := Some(2022)

// SBT Commands

// Calls prePR, but also the additional commands needed to run on the meta
// project defined in the project/ directory.
//
// Also adds scalafixAll, as that is not in prePR.
addCommandAlias(
  "prePRAll",
  s";+scalafixAll;+prePR;reload plugins;clean;scalafixAll;headerCreateAll;test")

addCommandAlias(
  "prePRLight",
  s";prePR;reload plugins;scalafixAll;headerCreateAll;"
)

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

// Scalafix

// Use the scalafix config which is valid for all versions of Scala if
// scalaVersion is different than the default one. Otherwise, use the full
// scalafix config, which includes Scala version specific rules.
ThisBuild / scalafixConfig := {
  if ((LocalRootProject / scalaVersion).value != DefaultScalaVersion) {
    Some(file(".scalafix-base.conf"))
  } else scalafixConfig.value
}

ThisBuild / ScalafixConfig / skip := tlIsScala3.value

// SBT Typelevel Github Actions

ThisBuild / githubWorkflowJavaVersions := Seq(JavaSpec.temurin("17"))

ThisBuild / githubWorkflowGeneratedCI += WorkflowJob(
  id = "codegen",
  name = "Codegen Test/Lint",
  steps = List(
    WorkflowStep.Checkout,
    WorkflowStep.Sbt(commands =
      List("reload plugins", "headerCheckAll", "scalafixAll --check", "test"))),
  scalas = List(Scala212)
)

ThisBuild / githubWorkflowEnv ++= Map(
  "JAVA_TOOL_OPTIONS" -> "-XX:+UseG1GC -XX:MaxHeapFreeRatio=20 -Xmx4G -XX:MinHeapFreeRatio=10 -XX:+UseStringDeduplication"
)

// Projects

lazy val projectName: String = "idna4s"

lazy val root = tlCrossRootProject
  .aggregate(
    core,
    scalacheck,
    tests,
    benchmarks
  )
  .settings(name := projectName)

lazy val core = crossProject(JVMPlatform, JSPlatform, NativePlatform)
  .crossType(CrossType.Full)
  .in(file("core"))
  .settings(
    name := s"${projectName}-core",
    libraryDependencies ++= Seq(
      "org.typelevel" %%% "cats-collections-core" % catsCollectionsV,
      "org.typelevel" %%% "cats-core"             % catsV
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
      "org.scalameta" %%% "munit"            % munitV,
      "org.scalameta" %%% "munit-scalacheck" % munitScalacheckV,
      "org.typelevel" %%% "discipline-munit" % disciplineMunitV
    ).map(_ % Test),
    console / initialCommands := {
      List(
        "cats.",
        "cats.syntax.all.",
        "org.typelevel.idna4s.core.",
        "org.typelevel.idna4s.core.uts46.",
        "org.typelevel.idna4s.core.bootstring.",
        "org.typelevel.idna4s.core.syntax.all."
      ).map(value => s"import ${value}${wildcardImport.value}").mkString("\n")
    },
    consoleQuick / initialCommands := "",
    Compile / sourceGenerators ++= List(
      (Compile / sourceManaged)
        .map(
          CodeGen.generate(_, UnicodeVersion)
        )
        .taskValue
    )
  )

lazy val scalacheck = crossProject(JVMPlatform, JSPlatform, NativePlatform)
  .crossType(CrossType.Pure)
  .in(file("scalacheck"))
  .settings(
    name := s"${projectName}-scalacheck",
    libraryDependencies ++= Seq(
      "org.scalacheck" %%% "scalacheck" % scalacheckV
    ),
    console / initialCommands := {
      List(
        "cats.",
        "cats.syntax.all.",
        "org.scalacheck.",
        "org.typelevel.idna4s.core.",
        "org.typelevel.idna4s.core.bootstring.",
        "org.typelevel.idna4s.core.syntax.all.",
        "org.typelevel.idna4s.core.uts46.",
        "org.typelevel.idna4s.scalacheck.all."
      ).map(value => s"import ${value}${wildcardImport.value}").mkString("\n")
    },
    consoleQuick / initialCommands := ""
  )
  .dependsOn(core)

lazy val tests = crossProject(JVMPlatform, JSPlatform, NativePlatform)
  .crossType(CrossType.Full)
  .in(file("tests"))
  .settings(
    name := s"${projectName}-tests",
    libraryDependencies ++= Seq(
      "org.typelevel" %%% "discipline-munit" % disciplineMunitV,
      "org.typelevel" %%% "cats-laws"        % catsV
    ),
    Test / console / initialCommands := {
      List(
        "cats.",
        "cats.syntax.all.",
        "org.scalacheck.",
        "org.typelevel.idna4s.core.",
        "org.typelevel.idna4s.core.bootstring.",
        "org.typelevel.idna4s.core.syntax.all.",
        "org.typelevel.idna4s.core.uts46.",
        "org.typelevel.idna4s.scalacheck.all."
      ).map(value => s"import ${value}${wildcardImport.value}").mkString("\n")
    },
    consoleQuick / initialCommands := ""
  )
  .jvmSettings(
    libraryDependencies ++= Seq(
      "com.ibm.icu" % "icu4j" % icu4jV
    ).map(_ % Test)
  )
  .dependsOn(core % Test, scalacheck % Test)
  .enablePlugins(NoPublishPlugin)

lazy val benchmarks = project
  .in(file("benchmarks"))
  .settings(
    libraryDependencies ++= List(
      "com.ibm.icu" % "icu4j" % icu4jV
    ),
    console / initialCommands := {
      List(
        "cats.",
        "cats.syntax.all.",
        "org.scalacheck.",
        "org.typelevel.idna4s.core.",
        "org.typelevel.idna4s.core.bootstring.",
        "org.typelevel.idna4s.core.syntax.all.",
        "org.typelevel.idna4s.core.uts46.",
        "org.typelevel.idna4s.scalacheck.all."
      ).map(value => s"import ${value}${wildcardImport.value}").mkString("\n")
    },
    consoleQuick / initialCommands := ""
  )
  .dependsOn(core.jvm, scalacheck.jvm)
  .enablePlugins(NoPublishPlugin, JmhPlugin)
