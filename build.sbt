name := "free"

version := "0.1"

scalaVersion := "2.12.4"

libraryDependencies += "org.typelevel" %% "cats-free" % "1.1.0"
libraryDependencies += "org.typelevel" %% "cats-effect" % "1.0.0-RC"
libraryDependencies += "org.typelevel" %% "cats-mtl-core" % "0.2.3"

libraryDependencies += "io.frees" %% "frees-core" % "0.8.0"
libraryDependencies += "io.frees" %% "frees-effects" % "0.8.0"

scalacOptions ++= List("-Ypartial-unification", "-language:higherKinds")

addCompilerPlugin("org.scalameta" % "paradise" % "3.0.0-M11" cross CrossVersion.full)
addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.6")