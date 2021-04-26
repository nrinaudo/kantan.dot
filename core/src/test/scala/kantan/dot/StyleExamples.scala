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

import java.io.File

/** Collection of examples.
  *
  * This is half tests, half documentation. The test scenarios are extremely simple, but showcase most (all?) of the
  * behaviours you should expect when applying stylesheets.
  *
  * Scenarios can be consulted directly in the test resources, under `/examples`.
  */
class StylesheetExamples extends ExamplesSuite {

  def load[A: Parse](root: File, name: String): A = {
    val path = new File(root, name)

    Parse.parse[A](path).getOrElse(fail(s"Failed to load resource $path"))
  }

  testSeries("style") { example =>
    val graph      = load[Graph](example, "input.dot")
    val stylesheet = load[Stylesheet](example, "style.dss")
    val expected   = load[Graph](example, "expected.dot")

    Graphviz.eval(stylesheet.applyTo(graph), Graphviz.Format.Fig) should be(
      Graphviz.eval(expected, Graphviz.Format.Fig)
    )
  }
}
