addSbtPlugin("org.portable-scala" % "sbt-scalajs-crossproject" % "1.3.1")
addSbtPlugin("org.scala-js"       % "sbt-scalajs"              % "1.13.1")
addSbtPlugin("org.scalameta"      % "sbt-scalafmt"             % "2.5.0")
addSbtPlugin("ch.epfl.scala"      % "sbt-scalafix"             % "0.10.4")
addSbtPlugin("de.heikoseeberger"  % "sbt-header"               % "5.9.0")
addSbtPlugin("com.github.sbt"     % "sbt-ci-release"           % "1.5.12")

addSbtPlugin(
  "com.ibm.cloud.diesel" % "diesel-i18n-plugin" % "0.6.0"
) // Dependencies.dieselI18nVersion
