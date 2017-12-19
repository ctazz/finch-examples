
name := "finch-examples"

version := "0.01"

scalaVersion := "2.12.2"

libraryDependencies ++= Seq(

	"com.github.finagle"      %% "finch-core" % "0.12.0",
	"com.github.finagle"      %% "finch-circe" % "0.12.0",
	"com.twitter"             %% "twitter-server" % "1.26.0",
	"io.circe"                %% "circe-generic" % "0.8.0",
	"io.circe"                %% "circe-parser" % "0.8.0",
	"io.circe"                %% "circe-java8" % "0.8.0"
)
