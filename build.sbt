import java.nio.file.{Files, Paths}
import scala.jdk.CollectionConverters.*

val scala3Version = "3.5.2"

lazy val root = project
  .in(file("."))
  .settings(
    name := "advent-of-code-2024",
    version := "0.1.0-SNAPSHOT",

    scalacOptions ++= Seq(
      "-encoding", "utf8",
      "-no-indent"
    ),

    scalaVersion := scala3Version,

    libraryDependencies += "org.scalameta" %% "munit" % "1.0.0" % Test
  )


lazy val generateTaskRegistry = taskKey[Seq[java.io.File]]("Generates TaskRegistry.scala dynamically")

generateTaskRegistry := {
  val log = streams.value.log
  val baseDir = (Compile / sourceDirectory).value / "scala"
  val outputDir = (Compile / sourceManaged).value / "taskregistry"
  val registryFile = outputDir / "TaskRegistry.scala"

  // Ensure the output directory exists
  if (!outputDir.exists()) outputDir.mkdirs()

  // Find all files matching "Day*.scala"
  val dayFiles = Files
    .walk(baseDir.toPath)
    .sorted()
    .iterator()
    .asScala
    .filter(_.getFileName.toString.matches("Day\\d+\\.scala"))
    .toList

  log.info(s"Discovered task files: ${dayFiles.mkString(", ")}")

  // Generate object mappings
  val mappings = dayFiles.map { path =>
    val name = path.getFileName.toString.stripSuffix(".scala")
    s""""$name" -> $name"""
  }.mkString(",\n    ")

  // Generate the Scala source file
  val content =
    s"""
       |// AUTO-GENERATED FILE: DO NOT EDIT
       |package advent.days
       |
       |import advent.days.*
       |import advent.Task
       |
       |object TaskRegistry {
       |  val tasks: Map[String, Task] = Map(
       |    $mappings
       |  )
       |}
       |""".stripMargin

  // Write the generated file
  IO.write(registryFile, content)
  log.info(s"Generated TaskRegistry.scala at: $registryFile")

  // Return the generated file as a sequence
  Seq(registryFile)
}

Compile / sourceGenerators += generateTaskRegistry.taskValue