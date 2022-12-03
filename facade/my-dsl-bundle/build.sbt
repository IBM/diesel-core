enablePlugins(ScalaJSPlugin)

scalaJSLinkerConfig ~= { _.withModuleKind(ModuleKind.CommonJSModule) }
Test / scalaJSLinkerConfig ~= { _.withModuleKind(ModuleKind.CommonJSModule) }

name := "my-dsl-bundle"

publish / skip := true

//// TODO?
scalacOptions ++= Seq(
  "-unchecked",
  "-deprecation",
  "-feature",
  "-Xfatal-warnings",
  "-language:existentials",
  "-Wunused:imports"
)

Compile / fastOptJS := {
  val file = (Compile / fastOptJS).value
  IO.copyFile(file.data, baseDirectory.value / "dist" / "my-dsl-bundle.js")
  file
}

cleanFiles += baseDirectory.value / "dist"
