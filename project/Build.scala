import sbt._
import Keys._

object Build extends Build {
  val sharedSettings = Defaults.defaultSettings ++ Seq(
    organization := "org.smop.experiments",
    version := "0.1.0-SNAPSHOT",
    scalaVersion := "2.10.0-M3",
    scalacOptions ++= Seq("-unchecked", "-Ymacro-debug-lite")
  )

  lazy val macros = Project(id = "macros", base = file("macros"), settings = sharedSettings ++ Seq(name := "macros"))

  lazy val root = Project(
    id = "experiments",
    base = file("."),
    settings = sharedSettings ++ Seq(
      name := "experiments",
      libraryDependencies ++= Seq(
        "com.codecommit" % "anti-xml_2.10.0-M3" % "0.4-SNAPSHOT"
      ),
      resolvers += "sonatype snapshots" at "https://oss.sonatype.org/content/repositories/snapshots/"
    )) dependsOn(macros)
}
