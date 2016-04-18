package com.blstream.sbtsearchmavenplugin

import org.specs2.mutable.Specification
import org.specs2.specification.{ AfterEach, BeforeEach }

class AddDependencyTest extends Specification with BeforeEach with AfterEach with AddDependencyTraits {
  sequential

  var chooseSequence = Seq(1, 2, 1)

  val artifactsList = List(Artifact("org.scalaz", "z", "1"), Artifact("foo", "bar", "baz"))

  override def inputFunction: Option[String => Option[String]] = {
    val chooseFn = choose(chooseSequence.head)
    chooseSequence = chooseSequence.tail
    chooseFn
  }

  "chooseArtifact" should {
    "allow a user to pick the first artifact out of a list of artifacts" >> {
      val expectedArtifact = artifactsList(chooseSequence.head - 1)
      val choosenArtifact = chooseArtifact(artifactsList)

      choosenArtifact must beEqualTo(Right(expectedArtifact))
    }
  }

  "chooseArtifact" should {
    "allow a user to pick the second artifact out of a list of artifacts" >> {
      val expectedArtifact = artifactsList(chooseSequence.head - 1)
      val choosenArtifact = chooseArtifact(artifactsList)

      choosenArtifact must beEqualTo(Right(expectedArtifact))
    }
  }

  def choose(index: Integer): Option[String => Option[String]] = {
    def chooseIndex(str: String): Option[String] = {
      Some(s"""$index""")
    }
    Some(chooseIndex)
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

