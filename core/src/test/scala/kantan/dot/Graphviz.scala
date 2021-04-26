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

package kantan.dot

import java.io.{ByteArrayInputStream, File}
import sys.process._

object Graphviz {

  sealed abstract class Format(val param: String) extends Product with Serializable

  object Format {
    final case object Canon extends Format("canon")
    final case object Fig   extends Format("fig")
  }

  def eval(input: String, format: Format): String =
    (s"dot -q -T${format.param}" #< new ByteArrayInputStream(input.getBytes("UTF-8"))).!!

  def eval(input: File, format: Format): String =
    (s"dot -q -T${format.param}" #< input).!!

  def eval(graph: Graph, format: Format): String = eval(Print.toString(graph), format)

}
