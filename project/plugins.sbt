val artifactoryHost = "eu.artifactory.swg-devops.com"
val username        = sys.env.getOrElse("ARTIFACTORY_USERNAME", "missing-ARTIFACTORY_USERNAME")
val password        =
  sys.env.getOrElse("ARTIFACTORY_PASSWORD", "missing-ARTIFACTORY_PASSWORD") // API token

val resolverUrl = s"https://$artifactoryHost/artifactory/hyc-diesel-team-maven-virtual/"

resolvers += "Artifactory" at resolverUrl
credentials += Credentials("Artifactory Realm", artifactoryHost, s"${username}", s"${password}")

addSbtPlugin("org.portable-scala" % "sbt-scalajs-crossproject" % "1.2.0")
addSbtPlugin("org.scala-js"       % "sbt-scalajs"              % "1.11.0")
addSbtPlugin("org.scalameta"      % "sbt-scalafmt"             % "2.5.0")
addSbtPlugin("ch.epfl.scala"      % "sbt-scalafix"             % "0.10.4")
addSbtPlugin("org.scoverage"      % "sbt-scoverage"            % "2.0.6")
addSbtPlugin(
  "com.ibm.diesel"                % "diesel-i18n-plugin"       % "LATEST-SNAPSHOT"
) // Dependencies.dieselI18nVersion
addSbtPlugin("de.heikoseeberger" % "sbt-header" % "5.9.0")
