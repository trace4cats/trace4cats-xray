lazy val commonSettings = Seq(
  Compile / compile / javacOptions ++= Seq("-source", "1.8", "-target", "1.8"),
  libraryDependencies ++= {
    CrossVersion.partialVersion(scalaVersion.value) match {
      case Some((2, _)) =>
        Seq(compilerPlugin(Dependencies.kindProjector), compilerPlugin(Dependencies.betterMonadicFor))
      case _ => Seq.empty
    }
  },
  scalacOptions += "-Wconf:any:wv",
  Test / fork := true,
  resolvers += Resolver.sonatypeRepo("releases"),
)

lazy val noPublishSettings =
  commonSettings ++ Seq(publish := {}, publishArtifact := false, publishTo := None, publish / skip := true)

lazy val publishSettings = commonSettings ++ Seq(
  publishMavenStyle := true,
  pomIncludeRepository := { _ =>
    false
  },
  Test / publishArtifact := false
)

lazy val root = (project in file("."))
  .settings(noPublishSettings)
  .settings(name := "Trace4Cats AWS X-Ray")
  .aggregate(`xray-udp-exporter`)

lazy val `xray-udp-exporter` =
  (project in file("modules/xray-udp-exporter"))
    .settings(publishSettings)
    .settings(
      name := "trace4cats-xray-udp-exporter",
      libraryDependencies ++= Seq(
        Dependencies.circeGeneric,
        Dependencies.fs2Io,
        Dependencies.trace4catsModel,
        Dependencies.trace4catsKernel,
        Dependencies.trace4catsExporterCommon
      )
    )
