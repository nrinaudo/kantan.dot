addSbtPlugin("com.github.gseitz" % "sbt-release"            % "1.0.13")
addSbtPlugin("com.jsuereth"      % "sbt-pgp"                % "2.0.1")
addSbtPlugin("org.scalameta"     % "sbt-scalafmt"           % "2.4.2")
addSbtPlugin("de.heikoseeberger" % "sbt-header"             % "5.6.0")
addSbtPlugin("org.scalastyle"    %% "scalastyle-sbt-plugin" % "1.0.0")
addSbtPlugin("org.wartremover"   % "sbt-wartremover"        % "2.4.13")
addSbtPlugin("org.xerial.sbt"    % "sbt-sonatype"           % "3.9.5")
addSbtPlugin("com.eed3si9n"      % "sbt-buildinfo"          % "0.9.0")

libraryDependencies += "org.scala-sbt" %% "scripted-plugin" % sbtVersion.value
