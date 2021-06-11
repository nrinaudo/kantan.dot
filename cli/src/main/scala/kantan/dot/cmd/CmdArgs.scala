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
import scopt.OParser

@SuppressWarnings(Array("org.wartremover.warts.DefaultArguments"))
final case class CmdArgs(
  dot: Option[File] = None,
  dss: Option[File] = None,
  out: Option[File] = None
)

object CmdArgs {

  private val parser = {
    val builder = OParser.builder[CmdArgs]
    import builder._

    def isFile(file: File) =
      if(file.isFile) success
      else failure(s"$file is not a valid file.")

    OParser.sequence(
      programName("dss"),
      head("dss", BuildInfo.version),
      opt[File]('s', "style")
        .required()
        .validate(isFile)
        .action((file, args) => args.copy(dss = Some(file)))
        .text("stylesheet to apply"),
      opt[File]('i', "input")
        .required()
        .validate(isFile)
        .action((file, args) => args.copy(dot = Some(file)))
        .text("DOT file to apply style to"),
      opt[File]('o', "output")
        .validate(isFile)
        .action((file, args) => args.copy(out = Some(file)))
        .text("where to write the resulting DOT content (defaults to STDOUT)"),
      help("help"),
      version("version")
    )
  }

  def parse(args: Array[String]): Option[CmdArgs] =
    OParser.parse(parser, args, CmdArgs())
}
