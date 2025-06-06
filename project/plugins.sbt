import org.typelevel.idna4s.build.Versions

addSbtPlugin("org.scala-js"       % "sbt-scalajs"                   % "1.18.2")
addSbtPlugin("org.portable-scala" % "sbt-scala-native-crossproject" % "1.3.2")
addSbtPlugin("org.scala-native"   % "sbt-scala-native"              % "0.5.6")
addSbtPlugin("org.typelevel"      % "sbt-typelevel"          % Versions.SbtTypelevelVersion)
addSbtPlugin("org.typelevel"      % "sbt-typelevel-site"     % Versions.SbtTypelevelVersion)
addSbtPlugin("org.typelevel"      % "sbt-typelevel-scalafix" % Versions.SbtTypelevelVersion)
addSbtPlugin("pl.project13.scala" % "sbt-jmh"                % "0.4.7")
