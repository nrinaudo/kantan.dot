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

package kantan.dot.cli

import java.io.File
import kantan.dot.{Graph, Parse, Print, Stylesheet}

object Main extends App {

  def style(dot: File, dss: File): Graph = {
    val graph = Parse.parse[Graph](dot).getOrElse(sys.error(s"Failed to load DOT file $dot"))
    val style = Parse.parse[Stylesheet](dss).getOrElse(sys.error(s"Failed to load DSS file $dss"))

    style.applyTo(graph)
  }

  CmdArgs.parse(args) match {
    case Some(CmdArgs(Some(dot), Some(dss), out)) =>
      val output = style(dot, dss)

      out match {
        case Some(file) => Print.print(output, file)
        case None       => Print.print(output)
      }

    case _ =>
  }
}
