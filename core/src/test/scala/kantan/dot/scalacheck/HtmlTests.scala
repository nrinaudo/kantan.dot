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

package kantan.dot.scalacheck

import kantan.dot.{Parse, Print}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

class HtmlTests extends AnyFunSuite with Matchers with ScalaCheckDrivenPropertyChecks {

  def normalize(html: Html): Html = {
    def normalizeContent(content: List[Html]): List[Html] = content match {
      // Edge case 1: remove empty text blocks
      case Html.Text(text) :: tail if text.isEmpty => normalizeContent(tail)

      // Edge case 2: merge two consecutive text blocks.
      case Html.Text(t1) :: Html.Text(t2) :: tail => normalizeContent(Html.Text(t1 + t2) :: tail)

      case head :: tail => normalize(head) :: normalizeContent(tail)
      case Nil          => Nil
    }

    html match {
      case Html.Node(name, content) => Html.Node(name, normalizeContent(content))
      case Html.Text(text)          => Html.Text(text.trim)
    }
  }

  test("HTML roundtrip") {
    forAll { (html: Html) =>
      Parse.parse[Html](Print.toString(html)).map(normalize) should be(Right(normalize(html)))
    }
  }
}
