name := "pickler-visualizer"

organization := "org.improving"

scalaVersion := "2.10.0-SNAPSHOT"

// crossScalaVersions in GlobalScope <<= (scalaVersion)(v => Seq("2.9.1.RC4", v))

libraryDependencies <++= (scalaVersion)(v => Seq(
  "org.scala-lang" % "scala-library" % v,
  "org.scala-lang" % "scala-compiler" % v
))

fork in run := true

offline := true