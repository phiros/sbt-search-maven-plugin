/* This code is mostly based on SelectMainClass.scala written by
 * Mark Harrah in 2009 for the sbt project.
 */

package com.blstream.sbtsearchmavenplugin

object SelectDependency {
  // Some(SimpleReader.readLine _)
  def apply(promptIfMultipleChoices: Option[String => Option[String]], artifacts: Seq[Artifact]): Option[Artifact] = {
    artifacts.toList match {
      case Nil => None
      case head :: Nil => Some(head)
      case multiple =>
        promptIfMultipleChoices flatMap { prompt =>
          println("\nMultiple dependencies detected, select one to be used:\n")
          for ((artifact, index) <- multiple.zipWithIndex)
            println(" [" + (index + 1) + "] " + artifact)
          val line = trim(prompt("\nEnter number: "))
          println("")
          toInt(line, multiple.length) map multiple.apply
        }
    }
  }

  private def trim(s: Option[String]) = s.getOrElse("")

  private def toInt(s: String, size: Int) =
    try {
      val i = s.toInt
      if (i > 0 && i <= size)
        Some(i - 1)
      else {
        println("Number out of range: was " + i + ", expected number between 1 and " + size)
        None
      }
    } catch {
      case nfe: NumberFormatException =>
        println("Invalid number: " + nfe.toString)
        None
    }

}
