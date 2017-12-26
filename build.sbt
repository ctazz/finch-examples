
name := "finch-examples"

version := "0.01"

scalaVersion := "2.12.4"

libraryDependencies ++= Seq(

	"com.github.finagle"      %% "finch-core" % "0.16.0-M5",
	"com.github.finagle"      %% "finch-circe" % "0.16.0-M5",
	"com.twitter"             %% "twitter-server" % "17.12.0",
	"io.circe"                %% "circe-generic" % "0.9.0-M2",
	"io.circe"                %% "circe-parser" % "0.9.0-M2",
	"io.circe"                %% "circe-java8" % "0.9.0-M2",
	"org.typelevel" %% "cats-core" % "1.0.0-RC2",
	"org.typelevel" %% "cats-effect" % "0.5",

	//0.10.0-M9 depends on cats.Effect.sequence, which must've been removed at some point, and isn't in cats-effect 0.5:
	//java.lang.NoSuchMethodError: cats.effect.Effect.sequence(Ljava/lang/Object;Lcats/Traverse;)Ljava/lang/Object
	"co.fs2"                  %% "fs2-core"           % "0.10.0-M10",
	"co.fs2"                  %% "fs2-cats"           % "0.5.0",

	"org.scalatest" %% "scalatest" % "3.0.4" % "test"
)

Keys.mainClass in (Compile) := Some("org.example.finch.Main")