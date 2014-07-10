import sbt._
//import com.github.siasia._
import Keys._
import sbt.Package._
import sbtrelease._
import sbtrelease.ReleasePlugin._
import net.virtualvoid.sbt.graph.Plugin.graphSettings
import sbtrelease.ReleasePlugin.ReleaseKeys._
import sbtrelease.ReleaseStateTransformations._
import aether.Aether._


object TopLevelBuild extends Build {
  import BuildSettings._

  lazy val buildSettings = Seq(
    organization := "net.thecoda.sandbox",
    scalaVersion := "2.11.1",
    scalacOptions := Seq(
      "-feature",
      "-deprecation",
      "-unchecked",
      "-Xlint",
      "-encoding", "utf8",
      "-Yrangepos",
      "-Yno-adapted-args",
      "-Ywarn-dead-code",
      "-Ywarn-numeric-widen",   
      "-Ywarn-value-discard"),
    scalacOptions in (console) += "-Yrangepos"
    //scalacOptions in (Compile, compile) += "-P:wartremover:traverser:org.brianmckenna.wartremover.warts.Unsafe"
  )

  lazy val root = Project(id = "root", base = file("."))
    .aggregate(macros, core, cli)
    .settings(commonSettings : _*)

  lazy val macros = Project(id = "macros", base = file("macros"))
    .configs(IntegrationTest)
    .settings(commonSettings : _*)

  lazy val core = Project(id = "core", base = file("core"))
    .dependsOn(macros)
    .configs(IntegrationTest)
    .settings(commonSettings : _*)

  lazy val cli = Project(id = "cli", base = file("cli"))
    .dependsOn(core)
    .configs(IntegrationTest)
    .settings(commonSettings : _*)



  lazy val commonSettings =
    sbtPromptSettings ++
    buildSettings ++
    graphSettings ++
    releaseSettings ++
    Defaults.itSettings ++
    miscSettings
    //addCompilerPlugin("org.scalamacros" %% "paradise" % "2.1.0-M1")
    //addCompilerPlugin("org.brianmckenna" %% "wartremover" % "0.7")
    //publishSettings ++
    //releaseSettings ++
    //packagingSettings ++
    //aetherSettings ++
    //aetherPublishSettings ++

  lazy val miscSettings = Seq(
    resolvers ++= Resolvers.all,
    ivyXML := ivyDeps,
    autoCompilerPlugins := true,
    addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.0-M1" cross CrossVersion.full),
    libraryDependencies ++= Dependencies.core,
    libraryDependencies ++= Dependencies.test,
    libraryDependencies <++= (scalaVersion)(sv =>
      Seq(
        "org.scala-lang" % "scala-reflect" % sv,
        "org.scala-lang" % "scala-compiler" % sv
      )
    )
    //testOptions in Test += Tests.Argument("console") //, "junitxml")
  )

/*
  lazy val publishSettings: Seq[Setting[_]] = Seq(
    credentials += Credentials(Path.userHome / ".ivy2" / "zeebox.credentials"),
    publishMavenStyle := true,
    publishTo <<= version((v: String) =>
      Some(if (v.trim endsWith "SNAPSHOT") Resolvers.zeeboxSnapshots else Resolvers.zeeboxReleases)
    )
  )

  lazy val releaseSettings = Seq(
    releaseVersion := { ver => optEnv("GO_PIPELINE_COUNTER").getOrElse(versionFormatError) },
    nextVersion    := { ver => optEnv("GO_PIPELINE_COUNTER").getOrElse(versionFormatError) },
    releaseProcess := Seq[ReleaseStep](
      //checkSnapshotDependencies,
      inquireVersions,
      //runTest,
      setReleaseVersion,
      commitReleaseVersion, // performs the initial git checks
      tagRelease,
      publishArtifacts     // checks whether `publishTo` is properly set up
      //setNextVersion,
      //commitNextVersion
      //pushChanges           // also checks that an upstream branch is properly configured
    )
  )

  lazy val packagingSettings = Seq(
    packageOptions <<= (Keys.version, Keys.name, Keys.artifact) map {
      (version: String, name: String, artifact: Artifact) =>
        Seq(ManifestAttributes(
          "Implementation-Vendor" -> "thecoda.net",
          "Implementation-Title" -> "sandbox",
          "Version" -> version,
          "Build-Number" -> optEnv("GO_PIPELINE_COUNTER").getOrElse("n/a"),
          "Group-Id" -> name,
          "Artifact-Id" -> artifact.name,
          "Git-SHA1" -> Git.hash,
          "Git-Branch" -> Git.branch,
          "Built-By" -> "Oompa-Loompas",
          "Build-Jdk" -> prop("java.version"),
          "Built-When" -> (new java.util.Date).toString,
          "Build-Machine" -> java.net.InetAddress.getLocalHost.getHostName
        )
      )
    }
  )
*/

  val ivyDeps = {
    <dependencies>
      <!-- commons logging is evil. It does bad, bad things to the classpath and must die. We use slf4j instead -->
        <exclude module="commons-logging"/>
      <!-- Akka dependencies must be excluded if transitivly included,
           replaced with corresponding atmos-akka-xxx -->
      
        <!--
        <exclude module="akka-actor"/>
        <exclude module="akka-remote"/>
        <exclude module="akka-slf4j"/>
        <exclude module="slf4j-simple"/>
        -->
      
      <!-- Flume dependencies can be excluded if Flume isn't used -->
        <exclude module="flume"/>
        <exclude module="googleColl"/>
        <exclude module="libthrift"/>
        <exclude module="hadoop-core"/>
    </dependencies>
  }
}
