addSbtPlugin("org.portable-scala" % "sbt-scalajs-crossproject" % "1.2.0")
addSbtPlugin("org.scala-js"       % "sbt-scalajs"              % "1.11.0")
addSbtPlugin("org.scalameta"      % "sbt-scalafmt"             % "2.5.0")
addSbtPlugin("ch.epfl.scala"      % "sbt-scalafix"             % "0.10.4")
addSbtPlugin(
  "com.ibm.diesel"                % "diesel-i18n-plugin"       % "LATEST-SNAPSHOT"
) // Dependencies.dieselI18nVersion
addSbtPlugin("de.heikoseeberger" % "sbt-header" % "5.9.0")
