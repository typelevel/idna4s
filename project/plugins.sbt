val sbtTypelevelV: String = "0.4.16"

addSbtPlugin("org.scala-js"       % "sbt-scalajs"                   % "1.10.1")
addSbtPlugin("org.portable-scala" % "sbt-scala-native-crossproject" % "1.2.0")
addSbtPlugin("org.scala-native"   % "sbt-scala-native"              % "0.4.7")
addSbtPlugin("org.typelevel"      % "sbt-typelevel"                 % sbtTypelevelV)
addSbtPlugin("org.typelevel"      % "sbt-typelevel-site"            % sbtTypelevelV)
addSbtPlugin("org.typelevel"      % "sbt-typelevel-scalafix"        % sbtTypelevelV)
addSbtPlugin("pl.project13.scala" % "sbt-jmh"                       % "0.4.3")
