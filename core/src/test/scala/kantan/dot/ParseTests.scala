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

import kantan.dot.scalacheck._
import org.scalatest.AppendedClues
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

class ParseTests extends AnyFunSuite with Matchers with ScalaCheckDrivenPropertyChecks with AppendedClues {

  test("Graph roundtrip") {
    forAll { (graph: Graph) =>
      Parse.parse[Graph](Print.toString(graph)) should be(Right(graph))
    }
  }

  test("Comparison with reference implementation") {
    forAll(smallGraph) { graph =>
      val Right(canonical) = Parse.parse[Graph](Graphviz.eval(graph, Graphviz.Format.Canon))
      val Right(roundtrip) = Parse.parse[Graph](Print.toString(canonical))
      canonical should be(roundtrip)
    }
  }

  test("Stylesheet roundtrip") {
    forAll { (sheet: Stylesheet) =>
      Parse.parse[Stylesheet](Print.toString(sheet)) should be(Right(sheet))
    }
  }
}
