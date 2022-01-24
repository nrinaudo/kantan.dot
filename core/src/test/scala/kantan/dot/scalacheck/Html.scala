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

import fastparse.JavaWhitespace._
import fastparse._
import java.io.Writer
import kantan.dot.{Parse, Print}
import org.scalacheck.{Arbitrary, Gen, Shrink}
import org.scalacheck.Shrink.shrink

sealed abstract class Html extends Product with Serializable

object Html {
  // - Types of supported HTML nodes -----------------------------------------------------------------------------------
  // -------------------------------------------------------------------------------------------------------------------
  final case class Node(name: String, content: List[Html]) extends Html

  final case class Text(value: String) extends Html

  // - Printing --------------------------------------------------------------------------------------------------------
  // -------------------------------------------------------------------------------------------------------------------
  private def printHtml(html: Html, out: Writer): Unit = html match {
    case Html.Node(name, Nil) =>
      out.write("<")
      out.write(name)
      out.write("/>")

    case Html.Node(name, content) =>
      out.write("<")
      out.write(name)
      out.write(">")
      content.foreach(printHtml(_, out))
      out.write("</")
      out.write(name)
      out.write(">")

    case Html.Text(text) =>
      out.write(text.replaceAll("<", "&lt;").replaceAll(">", "&gt;"))
  }

  implicit val print: Print[Html] = printHtml

  // - Parsing (used for shrinking graphs) -----------------------------------------------------------------------------
  // -------------------------------------------------------------------------------------------------------------------
  private object grammar {
    def text[_: P]: P[Html.Text] = CharsWhile(c => c != '<' && c != '>').!.map(Html.Text)

    def emptyNode[_: P]: P[Html.Node] = P("<") ~ kantan.dot.fastparse.identifier.map(Html.Node(_, List.empty)) ~ P("/>")

    def nodeContent[_: P]: P[Seq[Html]] = (node | text).rep

    def nonEmptyNode[_: P]: P[Html.Node] = {
      val start             = P("<") ~ kantan.dot.fastparse.identifier ~ P(">")
      def end(name: String) = P("</") ~ P(name).! ~ P(">")

      for {
        name    <- start
        content <- nodeContent
        _       <- end(name)
      } yield Html.Node(name, content.toList)
    }

    def node[_: P]: P[Html.Node] = emptyNode | nonEmptyNode

    def html[_: P]: P[Html] = Start ~ node ~ End
  }

  implicit val parse: Parse[Html] = Parse.fromFastParse(grammar.html(_))

  // - Random generation -----------------------------------------------------------------------------------------------
  // -------------------------------------------------------------------------------------------------------------------
  implicit val arb: Arbitrary[Html] = {

    val genEmptyNode = Gen.identifier.map(Html.Node(_, List.empty))

    val genText = Gen.stringOf(Gen.alphaNumChar).map(Html.Text)

    def genHtml(depth: Int): Gen[Html] =
      if(depth == 0) Gen.oneOf(genEmptyNode, genText)
      else Gen.oneOf(genText, genNode(depth))

    def genNode(depth: Int) =
      for {
        name    <- Gen.identifier
        content <- Gen.resize(10, Gen.listOf(genHtml(depth - 1)))
      } yield Html.Node(name, content)

    Arbitrary(genNode(4))
  }

  // - Shrinking -------------------------------------------------------------------------------------------------------
  // -------------------------------------------------------------------------------------------------------------------

  private def shrinkName(name: String) = {
    def valid(name: String) =
      name.forall(c => c.isLetter || c.isDigit) && name.headOption.map(_.isLetter).getOrElse(false)

    shrink(name).filter(valid)
  }

  implicit val shrinker: Shrink[Html] = Shrink {
    case Html.Node(name, content) =>
      shrinkName(name).map(Html.Node(_, content)) ++
        shrink(content).map(Html.Node(name, _))

    case Html.Text(text) => shrink(text).map(Html.Text)
  }
}
