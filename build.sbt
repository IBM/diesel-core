import sbt.{CrossVersion, ThisBuild}
import sbtcrossproject.CrossPlugin.autoImport.crossProject

import scala.sys.process._

val scalaVersion2 = "2.13.16"
// val scalaVersion3 = "3.2.1"

inThisBuild(Seq(
  organization     := "com.ibm.cloud.diesel",
  scalaVersion     := scalaVersion2,
  versionScheme    := Some("semver-spec"),
  description      := "Diesel is a library for creating and using languages easily.",
  startYear        := Some(2018),
  organizationName := "The Diesel Authors",
  licenses += ("Apache-2.0", new URL("https://www.apache.org/licenses/LICENSE-2.0.txt"))
))

// CI convenience
addCommandAlias("lint", "fmtCheck;fixCheck;headerCheckAll")
addCommandAlias("build", "compile;samplesBundle/fastOptJS")

// dev convenience
addCommandAlias("fmtCheck", "all scalafmtSbtCheck scalafmtCheckAll")
addCommandAlias("fixCheck", "scalafixAll --check")
addCommandAlias("fmt", "all scalafmtSbt scalafmtAll")
addCommandAlias("lintFix", "headerCreateAll;scalafixAll;fmt")
addCommandAlias("cleanJVM", "all diesel/clean samples/clean")
addCommandAlias("testJVM", "all diesel/test samples/test")
addCommandAlias("testJS", "all dieselJS/test samplesJS/test")

lazy val root = project
  .in(file("."))
  .aggregate(diesel.jvm, diesel.js, samples.jvm, samples.js, samplesBundle)
  .settings(
    name           := "diesel-core-root",
    publish / skip := true
  )

lazy val sharedSettings_scalac = Seq(
  scalacOptions ++= Seq(
    "-unchecked",
    "-deprecation",
    "-feature",
    // "-Xfatal-warnings",
    "-Wconf:cat=deprecation:i",
    "-language:existentials",
    "-Wunused:imports",
    "-Ytasty-reader"
  )
)

lazy val sharedSettings_test = Seq(
  Test / fork        := false,
  Test / logBuffered := false,
  // see https://github.com/scalameta/munit/blob/main/junit-interface/src/main/java/munit/internal/junitinterface/JUnitRunner.java
  // with logger:
  // Test / testOptions += Tests.Argument("+l", "--summary=1")
  // without logger:
  Test / testOptions += Tests.Argument("--summary=1")
)

lazy val sharedSettings_lint = Seq(
  ThisBuild / semanticdbEnabled := true,
  ThisBuild / semanticdbVersion := scalafixSemanticdb.revision
)

lazy val sharedJsSettings = Seq(
  scalacOptions += {
    val branch        =
      if (version.value.endsWith("SNAPSHOT")) {
        "develop"
      } else {
        version.value
      }
    val local: String = baseDirectory.value.getParentFile.getParentFile.toURI.toString
    val remote        = s"https://raw.github.ibm.com/IBM/diesel-core/$branch/"
    println(s"sourceURIs : \nLOCAL:$local\nREMOTE:$remote")
    s"-P:scalajs:mapSourceURI:$local->$remote"
  },
  // for dependency: pine
  libraryDependencies := libraryDependencies.value.filterNot(_.name == "scalajs-compiler"),
  addCompilerPlugin("org.scala-js" % "scalajs-compiler" % scalaJSVersion cross CrossVersion.patch)
)

lazy val diesel = crossProject(JSPlatform, JVMPlatform)
  .withoutSuffixFor(JVMPlatform)
  .enablePlugins(I18nPlugin)
  .settings(
    name          := "diesel-core",
    i18nDir       := file("./diesel/i18n"),
    i18nClassName := "diesel.I18nFiles"
  )
  .settings(sharedSettings_scalac)
  .settings(
    libraryDependencies ++= Seq(
      "com.ibm.cloud.diesel" %%% "diesel-i18n"   % Dependencies.dieselI18nVersion,
      scalaOrganization.value  % "scala-reflect" % scalaVersion.value,
      "org.scalameta"        %%% "munit"         % "1.1.1" % Test
    )
  )
  .settings(sharedSettings_test)
  .settings(sharedSettings_lint)
  .jsSettings(sharedJsSettings)

lazy val samples = crossProject(JSPlatform, JVMPlatform)
  .withoutSuffixFor(JVMPlatform)
  .in(file("./diesel-samples"))
  .settings(
    name := "diesel-core-samples"
  )
  .dependsOn(diesel % "compile->compile;test->test")
  .settings(sharedSettings_scalac)
  .settings(sharedSettings_test)
  .settings(sharedSettings_lint)
  .jsSettings(sharedJsSettings)

lazy val samplesBundle = project
  .in(file("./facade/samples-bundle"))
  .dependsOn(samples.js)
