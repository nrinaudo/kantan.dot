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

import java.io.File
import kantan.dot.{Graph, Parse, Stylesheet}

/** Stylesheet that tracks is date of last modification.
  *
  * The goal is to decide whether a DOT file should be re-processed or not.
  */
sealed trait TimedStylesheet {
  def applyTo(graph: Graph): Graph
  def youngerThan(file: File): Boolean
}

object TimedStylesheet {

  /** The empty stylesheet, whose modification time is _older than everything_. */
  val empty = new TimedStylesheet {
    override def applyTo(graph: Graph) = graph
    def youngerThan(file: File)        = false
  }

  /** Attempts to load the specified file as a stylesheet. */
  def fromFile(source: File): Either[String, TimedStylesheet] =
    Parse
      .parse[Stylesheet](source)
      .map(style =>
        new TimedStylesheet {
          override def applyTo(graph: Graph)   = style.applyTo(graph)
          override def youngerThan(file: File) = source.lastModified >= file.lastModified
        }
      )
}
