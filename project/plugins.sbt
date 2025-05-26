addSbtPlugin("org.portable-scala" % "sbt-scalajs-crossproject" % "1.3.2")
addSbtPlugin("org.scala-js"       % "sbt-scalajs"              % "1.19.0")
addSbtPlugin("org.scalameta"      % "sbt-scalafmt"             % "2.5.4")
addSbtPlugin("ch.epfl.scala"      % "sbt-scalafix"             % "0.14.3")
addSbtPlugin("de.heikoseeberger"  % "sbt-header"               % "5.10.0")
addSbtPlugin("com.github.sbt"     % "sbt-ci-release"           % "1.9.3")

addSbtPlugin(
  "com.ibm.cloud.diesel" % "diesel-i18n-plugin" % "0.7.0"
) // Dependencies.dieselI18nVersion

// until we get Mend/Whitesource to work:
// manual updates via "dependencyUpdates",
// see https://github.com/aiyanbo/sbt-dependency-updates
addSbtPlugin("org.jmotor.sbt" % "sbt-dependency-updates" % "1.2.9")
