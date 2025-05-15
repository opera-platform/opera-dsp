import Tests.*

val chisel6Version = "6.7.0"
val chiselTestVersion = "6.0.0"
val scalaVersionFromChisel = "2.13.16"

val chisel3Version = "3.6.1"

// This gives us a nicer handle to the root project instead of using the
// implicit one
lazy val projectRoot = project.in(file("."))

// keep chisel/firrtl specific class files, rename other conflicts
val chiselFirrtlMergeStrategy = CustomMergeStrategy.rename { dep =>
  import sbtassembly.Assembly.{Project, Library}
  val nm = dep match {
    case p: Project => p.name
    case l: Library => l.moduleCoord.name
  }
  if (Seq("firrtl", "chisel3", "chisel").contains(nm.split("_")(0))) { // split by _ to avoid checking on major/minor version
    dep.target
  } else {
    "renamed/" + dep.target
  }
}

lazy val commonSettings = Seq(
  scalaVersion := scalaVersionFromChisel,
  assembly / test := {},
  assembly / assemblyMergeStrategy := {
    case PathList("chisel3", "stage", xs @ _*) => chiselFirrtlMergeStrategy
    case PathList("chisel", "stage", xs @ _*) => chiselFirrtlMergeStrategy
    case PathList("firrtl", "stage", xs @ _*) => chiselFirrtlMergeStrategy
    case PathList("META-INF", _*) => MergeStrategy.discard
    // should be safe in JDK11: https://stackoverflow.com/questions/54834125/sbt-assembly-deduplicate-module-info-class
    case x if x.endsWith("module-info.class") => MergeStrategy.discard
    case x =>
      val oldStrategy = (assembly / assemblyMergeStrategy).value
      oldStrategy(x)
  },
  scalacOptions ++= Seq(
    "-deprecation",
    "-unchecked",
    "-Ytasty-reader",
    "-Ymacro-annotations"), // fix hierarchy API
  allDependencies := {
    // drop specific maven dependencies in subprojects in favor of local version
    val dropDeps = Seq(("edu.berkeley.cs", "rocketchip"))
    allDependencies.value.filterNot { dep =>
      dropDeps.contains((dep.organization, dep.name))
    }
  },
  libraryDependencies += "com.lihaoyi" %% "sourcecode" % "0.3.1",
  libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value,

  exportJars := true,
  resolvers ++= Resolver.sonatypeOssRepos("snapshots"),
  resolvers ++= Resolver.sonatypeOssRepos("releases"),
  resolvers ++= Seq(Resolver.mavenLocal)
)

val rocketChipDir = file("dependencies/rocket-chip")

/**
 * It has been a struggle for us to override settings in subprojects.
 * An example would be adding a dependency to rocketchip on midas's targetutils library,
 * or replacing dsptools's maven dependency on chisel with the local chisel project.
 *
 * This function works around this by specifying the project's root at src/ and overriding
 * scalaSource and resourceDirectory.
 */
def freshProject(name: String, dir: File): Project = {
  Project(id = name, base = dir / "src")
    .settings(
      Compile / scalaSource := baseDirectory.value / "main" / "scala",
      Compile / resourceDirectory := baseDirectory.value / "main" / "resources"
    )
}

// Fork each scala test for now, to work around persistent mutable state
// in Rocket-Chip based generators
def isolateAllTests(tests: Seq[TestDefinition]) = tests map { test =>
  val options = ForkOptions()
  new Group(test.name, Seq(test), SubProcess(options))
}


lazy val chisel6Settings = Seq(
  libraryDependencies ++= Seq("org.chipsalliance" %% "chisel" % chisel6Version),
  addCompilerPlugin("org.chipsalliance" % "chisel-plugin" % chisel6Version cross CrossVersion.full)
)
lazy val chisel3Settings = Seq(
  libraryDependencies ++= Seq("edu.berkeley.cs" %% "chisel3" % chisel3Version),
  addCompilerPlugin("edu.berkeley.cs" % "chisel3-plugin" % chisel3Version cross CrossVersion.full)
)

lazy val chiselSettings = chisel6Settings ++ Seq(
  libraryDependencies ++= Seq(
    "org.apache.commons" % "commons-lang3" % "3.12.0",
    "org.apache.commons" % "commons-text" % "1.9"
  )
)

lazy val scalaTestSettings =  Seq(
  libraryDependencies ++= Seq(
    "org.scalatest" %% "scalatest" % "3.2.+" % "test"
  )
)


// Subproject definitions begin
lazy val hardfloat = freshProject("hardfloat", rocketChipDir / "dependencies/hardfloat/hardfloat")
  .settings(chiselSettings)
  .settings(commonSettings)
  .settings(scalaTestSettings)

lazy val cde = (project in rocketChipDir / "dependencies/cde")
  .settings(commonSettings)
  .settings(Compile / scalaSource := baseDirectory.value / "cde/src/chipsalliance/rocketchip")

lazy val diplomacy = freshProject("diplomacy", rocketChipDir / "dependencies/diplomacy/diplomacy")
  .dependsOn(cde)
  .settings(commonSettings)
  .settings(chiselSettings)
  .settings(Compile / scalaSource := baseDirectory.value / "diplomacy")

lazy val rocketMacros  = (project in rocketChipDir / "macros")
  .settings(commonSettings)
  .settings(scalaTestSettings)

lazy val rocketchip = freshProject("rocketchip", rocketChipDir)
  .dependsOn(hardfloat, rocketMacros, diplomacy, cde)
  .settings(commonSettings)
  .settings(chiselSettings)
  .settings(scalaTestSettings)
  .settings(
    libraryDependencies ++= Seq(
      "com.lihaoyi" %% "mainargs" % "0.5.0",
      "org.json4s" %% "json4s-jackson" % "4.0.5",
      "org.scala-graph" %% "graph-core" % "1.13.5"
    )
  )
lazy val rocketLibDeps = rocketchip / Keys.libraryDependencies

lazy val fixedpoint = freshProject("fixedpoint", file("./dependencies/dsptools/fixedpoint"))
  .settings(chiselSettings)
  .settings(commonSettings)

lazy val dsptools = freshProject("dsptools", file("./dependencies/dsptools"))
  .dependsOn(fixedpoint)
  .settings(
    chiselSettings,
    commonSettings,
    scalaTestSettings,
    libraryDependencies ++= Seq(
      "edu.berkeley.cs" %% "chiseltest" % chiselTestVersion,
      "org.typelevel" %% "spire" % "0.18.0",
      "org.scalanlp" %% "breeze" % "2.1.0",
      "junit" % "junit" % "4.13" % "test",
      "org.scalacheck" %% "scalacheck" % "1.14.3" % "test",
    ))

lazy val rocket_dsp_utils = freshProject("rocket-dsp-utils", file("./dependencies/rocket-dsp-utils"))
  .dependsOn(rocketchip, cde, dsptools)
  .settings(libraryDependencies ++= rocketLibDeps.value)
  .settings(commonSettings)


lazy val preprocessing = (project in file("preprocessing"))
  .dependsOn(rocketchip, rocket_dsp_utils)
  .settings(
    libraryDependencies ++= rocketLibDeps.value,
    libraryDependencies ++= Seq("edu.berkeley.cs" %% "chiseltest" % chiselTestVersion),
    libraryDependencies ++= Seq("com.typesafe.play" %% "play-json" % "2.10.6")
  )
  .settings(commonSettings)

lazy val windowing = (project in file("windowing"))
  .dependsOn(rocketchip, rocket_dsp_utils)
  .settings(
    libraryDependencies ++= rocketLibDeps.value,
    libraryDependencies ++= Seq("edu.berkeley.cs" %% "chiseltest" % chiselTestVersion),
    libraryDependencies ++= Seq("com.typesafe.play" %% "play-json" % "2.10.6"),
    libraryDependencies ++= Seq("org.scalanlp" %% "breeze" % "2.1.0")
  )
  .settings(commonSettings)
