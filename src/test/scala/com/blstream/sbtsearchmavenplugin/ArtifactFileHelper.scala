package com.blstream.sbtsearchmavenplugin

import java.io.PrintWriter

trait ArtifactFileHelper {
  self: DependencyReaderWriter =>

  def getFileContents: String = {
    val string = scala.io.Source.fromFile(artifactFile).getLines.mkString("\n")
    string
  }

  def createEmptyFile: Unit = artifactFile.createNewFile()

  def createFileWithContents(contents: String): Unit = {
    createEmptyFile
    val writer = new PrintWriter(artifactFile)

    writer.write(contents)
    writer.close()
  }

  def before = artifactFile.delete()
  def after = artifactFile.delete()
}
