name := "csAir"
 
version := "0.1.0 "
 
scalaVersion := "2.9.2"
 
resolvers += "repo.codahale.com" at "http://repo.codahale.com"

libraryDependencies ++= Seq(
   "org.scalatest" %% "scalatest" % "1.6.1" % "test",
   "com.codahale" % "jerkson_2.9.1" % "0.5.0",
   "org.json4s" %% "json4s-native" % "3.1.0"
)
