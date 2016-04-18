package com.blstream.sbtsearchmavenplugin

import sbt.{ Project, SimpleReader, Logger }

import java.io.{ PrintWriter, File }

trait AddDependency {
  self: Search with DependencyReaderWriter with ArtifactString =>

  def add(args: Seq[String], log: Logger): Unit = {
    val results: Either[Error, Artifact] = getResultsFor(args, log) match {
      case Right((cleanedQuery, artifacts)) => {
        SelectDependency(Some(SimpleReader readLine _), artifacts) match {
          case Some(artifact: Artifact) => Right(artifact)
          case _ => Left("")
        }
      }
      case _ => Left("Couldn't find any dependency")
    }

    results.right.map(writeArtifactsFile(_))

    results.fold(
      log.warn(_),
      log.info(_)
    )
  }
}

trait ArtifactString {
  final val LIBRARY_DEPENDENCY_STRING: String = "libraryDependencies += "
  final val LIBRARY_DEPENDENCY_REGEX: String = "libraryDependencies \\+= "

  implicit def artifactToString(artifact: Artifact): String = {
    artifact match {
      case Artifact(a, g, v) => s"""$LIBRARY_DEPENDENCY_STRING"%s" %% "%s" %% "%s"""".format(a, g, v)
      case _ => ""
    }
  }

  implicit def stringToArtifact(str: String): Artifact = {
    str.replaceFirst(s"$LIBRARY_DEPENDENCY_REGEX", "").split('%').map(_.trim).toSeq match {
      case Seq(s: String, g: String, v: String) => Artifact(s.replaceAll("\"", ""), g.replaceAll("\"", ""), v.replaceAll("\"", ""))
      case _ => Artifact("", "", "")
    }
  }
}

trait DependencyReaderWriter {
  self: ArtifactString with sbt.PathExtra =>

  val artifactFile: File = Project("root", new File(".")).base / "ARTIFACTS.sbt"

  implicit def artifactsToFileString(artifacts: Seq[Artifact]): String = artifacts.map(a => a: String).mkString("\n")

  def writeArtifactsFile(artifact: Artifact): Unit = {
    val artifacts = readArtifactsFile match {
      case Some(artifacts: List[Artifact]) => insert(artifacts, artifact)
      case None => List(artifact)
    }
    val writer = new PrintWriter(artifactFile)
    writer.write(artifacts: String)
    writer.close()
  }

  private def insert(artifacts: List[Artifact], artifact: Artifact): List[Artifact] = {
    artifacts.filter(a => a == artifact) match {
      case List() => artifacts ::: List(artifact)
      case _ => artifacts
    }
  }

  def readArtifactsFile: Option[List[Artifact]] = {
    val sbtArtifactFormat = s"""$LIBRARY_DEPENDENCY_REGEX".*" \\% ".*" \\% ".*""""
    try {
      val foundArtifacts = scala.io.Source.fromFile(artifactFile)
        .getLines.filter(s => s.matches(sbtArtifactFormat))
        .map(a => a: Artifact).toList
      if (foundArtifacts.length > 0) Some(foundArtifacts)
      else None
    } catch {
      case _: Throwable => None
    }
  }
}
