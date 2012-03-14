scalaVersion := "2.9.1"

scalacOptions += "-deprecation"

resolvers += "processing github Maven Repository" at "https://github.com/alphaneet-debu/maven/raw/master"

libraryDependencies ++= Seq(
  "processing" %% "core" % "1.5.1"
)
