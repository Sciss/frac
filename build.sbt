name          := "frac"
version       := "1.1.0-SNAPSHOT"
organization  := "de.sciss" // "ca.frac"

scalaVersion  := "2.11.8"

scalacOptions := Seq("-deprecation", "-unchecked", "-feature", "-Xfuture", "-encoding", "utf8")

crossPaths    := false

resolvers += "scalaz-bintray" at "http://dl.bintray.com/scalaz/releases"

libraryDependencies ++= Seq(
  "de.sciss"        %% "swingplus"        % "0.2.1",
  "de.sciss"        %% "fileutil"         % "1.1.1",
  "de.sciss"        %% "equal"            % "0.1.1",
  "org.parboiled"   %% "parboiled-scala"  % "1.1.7",
  "org.specs2"      %% "specs2-core"      % "3.0.1" % "test"
)

mainClass in assembly := Some("frac.Main")

assemblyJarName in assembly := s"${name.value}-${version.value}.jar"

