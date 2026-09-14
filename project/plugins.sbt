addSbtPlugin("org.portable-scala" % "sbt-scalajs-crossproject" % "1.4.0")
addSbtPlugin("org.scala-js"       % "sbt-scalajs"              % "1.22.0")
addSbtPlugin("org.scalameta"      % "sbt-scalafmt"             % "2.6.2")
addSbtPlugin("ch.epfl.scala"      % "sbt-scalafix"             % "0.14.9")
addSbtPlugin("com.github.sbt"     % "sbt-header"               % "5.11.0")
addSbtPlugin("com.github.sbt"     % "sbt-ci-release"           % "1.12.1")

// Dependencies.dieselI18nVersion
addSbtPlugin(
  ("com.ibm.cloud.diesel" % "diesel-i18n-plugin" % "0.7.0+40-a85d92b3+20260914-1552-SNAPSHOT")
)
