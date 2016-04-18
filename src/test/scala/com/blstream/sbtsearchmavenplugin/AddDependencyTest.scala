package com.blstream.sbtsearchmavenplugin

import org.specs2.mutable.Specification
import org.specs2.specification.{AfterEach, BeforeEach}

class AddDependencyTest extends Specification with BeforeEach with AfterEach with AddDependencyTraits {
  sequential

  "add" should {
    "properly write a single artifact to an non existing artifact file" >> {
      val artifact = artifacts(0)
      writeArtifactsFile(artifact)
      val text = getFileContents
      val expectedResult =
        s"""|libraryDependencies += "org.scalaz" % "z" % "1"""".stripMargin

      text must beEqualTo(expectedResult)
    }
  }
}

trait AddDependencyTraits
  extends AddDependency
  with Search
  with MavenOrgSearcher
  with QueryCleaner
  with ResultsParser
  with ArtifactsPrinter
  with DependencyReaderWriter
  with ArtifactString
  with sbt.PathExtra
  with ArtifactFileHelper



