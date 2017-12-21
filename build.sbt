
name := "finch-examples"

version := "0.01"

scalaVersion := "2.12.4"

libraryDependencies ++= Seq(

	"com.github.finagle"      %% "finch-core" % "0.16.0-M5",
	"com.github.finagle"      %% "finch-circe" % "0.16.0-M5",
	"com.twitter"             %% "twitter-server" % "1.26.0",
	"io.circe"                %% "circe-generic" % "0.9.0-M2",
	"io.circe"                %% "circe-parser" % "0.9.0-M2",
	"io.circe"                %% "circe-java8" % "0.9.0-M2",

	"org.scalatest" %% "scalatest" % "3.0.4" % "test"
)
