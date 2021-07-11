lazy val root = Project(id = "kantan-dot", base = file("."))
  .settings(
    moduleName     := "root",
    publish / skip := true
  )
  .aggregate(core, plugin, sitePlugin, cli)
  .dependsOn(core)

lazy val core = project
  .enablePlugins(AutomateHeaderPlugin)
  .settings(
    moduleName := "kantan.dot",
    name       := "core",
    libraryDependencies ++= Seq(
      "com.lihaoyi"       %% "fastparse"       % Versions.fastparse,
      "org.scalacheck"    %% "scalacheck"      % Versions.scalacheck % "test",
      "org.scalatest"     %% "scalatest"       % Versions.scalatest % "test",
      "org.scalatestplus" %% "scalacheck-1-14" % Versions.scalatestPlusScalacheck % "test"
    )
  )

lazy val cli = project
  .enablePlugins(AutomateHeaderPlugin, BuildInfoPlugin)
  .settings(
    moduleName          := "kantan.dot-cli",
    name                := "cli",
    libraryDependencies += "com.github.scopt" %% "scopt" % Versions.scopt,
    buildInfoKeys       := Seq[BuildInfoKey](version),
    buildInfoPackage    := "kantan.dot.cli"
  )
  .dependsOn(core)

lazy val plugin = project
  .enablePlugins(AutomateHeaderPlugin, SbtPlugin)
  .settings(
    moduleName := "kantan.dot-sbt",
    name       := "plugin"
  )
  .dependsOn(core)

lazy val sitePlugin = project
  .enablePlugins(AutomateHeaderPlugin, SbtPlugin)
  .settings(
    moduleName := "kantan.dot-sbt-site",
    name       := "sitePlugin",
    addSbtPlugin("com.typesafe.sbt" % "sbt-site" % Versions.sbtSite)
  )
  .dependsOn(plugin)
