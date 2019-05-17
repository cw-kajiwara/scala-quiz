import scalariform.formatter.preferences._

name := "sandbox"

version := "1.0"

scalaVersion := "2.11.4"

scalacOptions ++= Seq("-encoding", "UTF-8", "-feature", "-unchecked", "-deprecation")

libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.7" % "test"

scalariformPreferences :=
      scalariformPreferences.value
        .setPreference(AlignParameters, true)
        .setPreference(AlignSingleLineCaseStatements, true)
        .setPreference(DoubleIndentConstructorArguments, true)
        .setPreference(DanglingCloseParenthesis, Preserve)
        .setPreference(MultilineScaladocCommentsStartOnFirstLine, false)

lazy val compileScalastyle = taskKey[Unit]("compileScalastyle")

compileScalastyle := org.scalastyle.sbt.ScalastylePlugin.autoImport.scalastyle.in(Compile).toTask("").value

compile in Compile := (compile in Compile dependsOn compileScalastyle).value
