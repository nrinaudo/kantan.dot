/*
 * Copyright 2021 Nicolas Rinaudo
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package kantan.dot.sbt

import java.io.ByteArrayInputStream
import kantan.dot.{Graph, Parse, Print}
import sbt.Keys._
import sbt._
import sys.process._

object GraphvizPlugin extends AutoPlugin {
  override def trigger = allRequirements

  object autoImport {
    lazy val Graphviz                = (config("Graphviz") extend Compile).hide
    lazy val graphvizSourceDirectory = settingKey[File]("Where to look for Graphviz sources")
    lazy val graphvizTargetDirectory = settingKey[File]("Where Graphviz output goes")
    lazy val graphvizStylesheet      = settingKey[Option[File]]("Stylesheet to apply to DOT files")
    lazy val graphviz                = taskKey[Seq[File]]("Compiles all graphviz files to SVG")
  }

  import autoImport._

  override lazy val projectSettings: Seq[Setting[_]] = Seq(
    graphvizSourceDirectory := sourceDirectory.value / "graphviz",
    graphvizTargetDirectory := target.value / "graphviz",
    graphvizStylesheet      := None,
    graphviz := processDirectory(
      streams.value,
      graphvizSourceDirectory.value,
      graphvizTargetDirectory.value,
      loadStylesheet(graphvizStylesheet.value)
    )
  )

  /** Sets the specified file's extension to `.svg`. */
  def changeExtension(file: File): File = {
    val name  = file.getName
    val index = name.lastIndexOf('.')

    val newName =
      if(index >= 0) name.substring(0, index)
      else name

    new File(file.getParent, newName ++ ".svg")
  }

  /** Writes the specified graph to the specified file as SVG data. */
  def runGraphviz(graph: Graph, out: File): String = {

    out.getParentFile.mkdirs()

    (s"dot -q -Tsvg -o ${out.getPath}" #< new ByteArrayInputStream(Print.toString(graph).getBytes("UTF-8"))).!!
  }

  /** Loads a stylesheet from the specified file, defaulting to the empty stylesheet if `None`. */
  def loadStylesheet(file: Option[File]): TimedStylesheet =
    file match {
      case Some(source) => TimedStylesheet.fromFile(source).getOrElse(sys.error("Failed to load stylesheet"))
      case None         => TimedStylesheet.empty
    }

  /** Processes a single file. */
  def processFile(streams: TaskStreams, in: File, out: File, style: TimedStylesheet): File = {
    streams.log.info(s"compiling: $in")

    Parse
      .parse[Graph](in)
      .map(style.applyTo)
      .map(runGraphviz(_, out))
      .left
      .foreach(error => sys.error(s"Failed with error $error"))

    out
  }

  /** Checks whether the specified input file must be reprocessed.
    *
    * The check is relatively naive: if either the input file or the stylesheet have been modified since the last time
    * the output was generated, we must reprocess.
    *
    * A better implementation would use some sort of hash but honestly, that's more work than I'm willing to put into
    * this (and the SBT cache API is apparently beyond me).
    */
  def mustReprocess(in: File, out: File, style: TimedStylesheet): Boolean =
    in.lastModified >= out.lastModified || style.youngerThan(out)

  def processDirectory(streams: TaskStreams, source: File, target: File, style: TimedStylesheet): Seq[File] = {
    streams.log.info("running graphviz")

    (source ** "*.dot")
      .pair(Path.rebase(source, target))
      .map { case (in, out) => (in, changeExtension(out)) }
      .map {
        case (in, out) =>
          if(mustReprocess(in, out, style))
            processFile(streams, in, changeExtension(out), style)
          out
      }
  }
}
