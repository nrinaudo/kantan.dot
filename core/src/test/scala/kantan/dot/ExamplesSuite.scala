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
import org.scalatest.Assertion
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

/** Test suite that reads examples from the `examples/` resource folder. */
abstract class ExamplesSuite extends AnyFunSuite with Matchers {

  def testSeries(name: String)(runTest: File => Assertion): Unit = {
    val root = new File(getClass.getClassLoader.getResource(s"examples/$name").getFile());

    for {
      category <- root.listFiles.toList
      example  <- category.listFiles.toList
    } test(s"${category.getName} / ${example.getName}") {
      runTest(example)
    }
  }
}
