lazy val root = project
  .in(file("."))
  .settings(
    name := "Stacky",
    description := "A micro virtual machine to analyze the JVM stack.",
    version := "1.0.0",
    scalaVersion := "3.1.0",

    resolvers += "jitpack" at "https://jitpack.io",
    libraryDependencies += "com.github.Hippo" % "HippoCafe" % "2.1.8",

    libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.9" % Test
  )