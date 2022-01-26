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

import org.scalacheck.{Arbitrary, Gen, Shrink}
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Shrink.shrink

package object scalacheck {
  // - Misc. -----------------------------------------------------------------------------------------------------------
  // -------------------------------------------------------------------------------------------------------------------

  /** Turns a generator into an arbitrary with a reasonable maximum depth.
    *
    * This is achieved by dividing the generator's size by 25, on the purely arbitrary notion that 4 seems like a good
    * default maximum depth and the default size is 100.
    */
  def arbWithDepth[A](f: Int => Gen[A]): Arbitrary[A] = Arbitrary(Gen.sized(size => f(size / 25)))

  // - Atom ------------------------------------------------------------------------------------------------------------
  // -------------------------------------------------------------------------------------------------------------------

  /** Alpha-numeric atoms cannot be keywords or empty. */
  def legalAlphaNum(str: String): Boolean = !(str.isEmpty || fastparse.keywords.contains(str))

  val alphaNum: Gen[String] =
    Gen.identifier
      .filter(legalAlphaNum)

  val quoted: Gen[String] = {
    val genChar = Gen.frequency(
      9 -> Gen.alphaNumChar,
      1 -> arbitrary[Char]
    )
    Gen
      .stringOf(genChar)
  }

  val num: Gen[String] = {
    val decimalOnly = Gen.posNum[Int].map(i => "." + i.toString)

    val integralOnly = Gen.posNum[Int].map(_.toString)

    val both = for {
      i <- integralOnly
      d <- decimalOnly
    } yield i + d

    for {
      sign    <- Gen.oneOf("-", "")
      content <- Gen.oneOf(integralOnly, decimalOnly, both)
    } yield sign ++ content
  }

  val atom: Gen[String] = Gen.frequency(
    8 -> alphaNum,
    1 -> num,
    1 -> quoted
  )

  val id: Gen[Id] = Gen.oneOf(
    atom.map(Id.Text.apply),
    arbitrary[Html].map(htmlToId)
  )

  private def htmlToId(html: Html): Id = Id.Html(Print.toString(html))

  private implicit val arbId: Arbitrary[Id] = Arbitrary(id)

  private implicit val shrinkId: Shrink[Id] = {
    implicit val shrinkChar: Shrink[Char] = Shrink.shrinkAny

    Shrink {
      case Id.Text(str) => shrink(str.toList).map(cs => Id.Text(cs.mkString))
      case Id.Html(str) =>
        Parse.parse[Html](str) match {
          case Left(_)     => Stream.empty
          case Right(html) => shrink(html).map(htmlToId)
        }
    }
  }

  private def nonEmpty(id: Id) = id match {
    case Id.Text(str) => str.nonEmpty
    case Id.Html(str) => str.nonEmpty
  }

  private implicit val shrinkAttribute: Shrink[(Id, Id)] = Shrink {
    case (key, value) =>
      shrink(key).filter(nonEmpty).map(k => (k, value)) #:::
        shrink(value).map(v => (key, v))
  }

  private implicit val shrinkAttributes: Shrink[Map[Id, Id]] = Shrink { map =>
    shrink(map.toList).map(_.toMap)
  }

  // - Node ------------------------------------------------------------------------------------------------------------
  // -------------------------------------------------------------------------------------------------------------------
  val rawNodeId: Gen[Id] = Gen.sized(size => Gen.resize(size / 10, id)).filter(nonEmpty)

  val compassPoint: Gen[Id.Text] =
    Gen.oneOf("n", "ne", "e", "se", "s", "sw", "w", "nw", "c", "_").map(Id.Text.apply)

  val port: Gen[Port] = {
    val simple = compassPoint.map(Port.Simple)

    val compound = for {
      id    <- rawNodeId
      point <- compassPoint
    } yield Port.Compound(id, point)

    Gen.frequency(
      5 -> Gen.const(Port.None),
      3 -> simple,
      2 -> compound
    )
  }

  implicit val arbPort: Arbitrary[Port] = Arbitrary(port)

  implicit val shrinkPort: Shrink[Port] = Shrink {
    case Port.Simple(id) =>
      Port.None #:: shrink(id).filter(nonEmpty).map(Port.Simple)

    case port: Port.Compound =>
      Port.None #:: Port.Simple(port.id) #::
        shrink(port.id).map(id => port.copy(id = id)) #:::
        shrink(port.point).map(point => port.copy(point = point))

    case Port.None => Stream.empty
  }

  val nodeId: Gen[NodeId] = for {
    id   <- rawNodeId
    port <- arbitrary[Port]
  } yield NodeId(id, port)

  implicit val arbNodeId: Arbitrary[NodeId] = Arbitrary(nodeId)

  implicit val shrinkNodeId: Shrink[NodeId] = Shrink { nodeId =>
    shrink(nodeId.id).filter(nonEmpty).map(id => nodeId.copy(id = id)) #:::
      shrink(nodeId.port).map(port => nodeId.copy(port = port))
  }

  val node: Gen[Node] = for {
    id         <- rawNodeId
    attributes <- attributes.forNodes
  } yield Node(id, attributes)

  implicit val arbNode: Arbitrary[Node] = Arbitrary(node)

  implicit val shrinkNode: Shrink[Node] = Shrink { node =>
    shrink(node.id).map(id => node.copy(id = id)) #:::
      shrink(node.attributes).map(attributes => node.copy(attributes = attributes))
  }

  // - Global attributes -----------------------------------------------------------------------------------------------
  // -------------------------------------------------------------------------------------------------------------------
  implicit val arbGlobalAttributes: Arbitrary[Attributes] = Arbitrary {
    Gen.oneOf(
      attributes.forNodes.filter(_.nonEmpty).map(Attributes.Node.apply _),
      attributes.forEdges.filter(_.nonEmpty).map(Attributes.Edge.apply _)
    )
  }

  implicit val shrinkGlobalAttributes: Shrink[Attributes] = Shrink {
    case Attributes.Node(attributes) =>
      shrink(attributes).filter(_.nonEmpty).map(Attributes.Node.apply _)
    case Attributes.Edge(attributes) =>
      shrink(attributes).filter(_.nonEmpty).map(Attributes.Edge.apply _)
  }

  // - Statement -------------------------------------------------------------------------------------------------------
  // -------------------------------------------------------------------------------------------------------------------
  def statement(depth: Int): Gen[Statement] = Gen.frequency(
    7 -> element(depth),
    3 -> arbitrary[Attributes]
  )

  implicit val shrinkStatement: Shrink[Statement] = Shrink {
    case attr: Attributes => shrink(attr)
    case element: Element => shrink(element)
  }

  // - Graph content ---------------------------------------------------------------------------------------------------
  // -------------------------------------------------------------------------------------------------------------------
  /** Generates random graph content of at most the specified depth.
    *
    * Note that this is written so that the deeper you are, the smaller the content.
    */
  def graphContent(depth: Int): Gen[GraphContent] =
    for {
      statements <- Gen.sized(size => Gen.resize(size / (5 - depth), Gen.listOf(statement(depth))))
      attributes <- attributes.forGraphs
    } yield GraphContent(attributes, statements)

  implicit val arbGraphContent: Arbitrary[GraphContent] = arbWithDepth(graphContent)
  implicit val shrinkGraphContent: Shrink[GraphContent] = Shrink { content =>
    shrink(content.statements).map(statements => content.copy(statements = statements)) #:::
      shrink(content.attributes).map(attributes => content.copy(attributes = attributes))
  }

  // - Subgraph --------------------------------------------------------------------------------------------------------
  // -------------------------------------------------------------------------------------------------------------------
  def subgraph(depth: Int): Gen[Subgraph] =
    for {
      id      <- arbitrary[Option[Id]]
      content <- graphContent(depth)
    } yield Subgraph(id, content)

  implicit val arbSubgraph: Arbitrary[Subgraph] = arbWithDepth(subgraph)

  implicit val shrinkSubgraph: Shrink[Subgraph] = Shrink { subgraph =>
    shrink(subgraph.id).map(id => subgraph.copy(id = id)) #:::
      shrink(subgraph.content).map(content => subgraph.copy(content = content))
  }

  // - Edge ------------------------------------------------------------------------------------------------------------
  // -------------------------------------------------------------------------------------------------------------------

  implicit val arbEdge: Arbitrary[Edge] = Arbitrary {
    for {
      head       <- arbitrary[NodeId]
      tail       <- arbitrary[NodeId]
      attributes <- attributes.forEdges
    } yield Edge(head, tail, attributes)
  }

  implicit val shrinkEdge: Shrink[Edge] = Shrink { edge =>
    shrink(edge.head).map(head => edge.copy(head = head)) #:::
      shrink(edge.tail).map(tail => edge.copy(tail = tail)) #:::
      shrink(edge.attributes).map(attributes => edge.copy(attributes = attributes))
  }

  // - Element ---------------------------------------------------------------------------------------------------------
  // -------------------------------------------------------------------------------------------------------------------
  def element(depth: Int): Gen[Element] =
    if(depth > 0)
      Gen.frequency(
        5 -> arbitrary[Node],
        4 -> arbitrary[Edge],
        1 -> subgraph(depth - 1)
      )
    else Gen.oneOf(arbitrary[Node], arbitrary[Edge])

  implicit val arbElement: Arbitrary[Element] = arbWithDepth(element)

  implicit val shrinkElement: Shrink[Element] = Shrink {
    case node: Node         => shrink(node)
    case subgraph: Subgraph => shrink(subgraph)
    case edge: Edge         => shrink(edge)
  }

  // - Selector --------------------------------------------------------------------------------------------------------
  // -------------------------------------------------------------------------------------------------------------------
  val classes: Gen[Set[String]] = Gen.buildableOf[Set[String], String](Gen.identifier)

  val entity: Gen[Entity] = Gen.oneOf(Entity.Node, Entity.Edge, Entity.Graph)

  implicit val arbEntity: Arbitrary[Entity] = Arbitrary(entity)

  implicit val arbSelector: Arbitrary[Selector] = Arbitrary {
    for {
      entity  <- arbitrary[Entity]
      classes <- classes
    } yield Selector(entity, classes)
  }

  implicit val shrinkSelector: Shrink[Selector] = Shrink { selector =>
    shrink(selector.entity).map(entity => selector.copy(entity = entity)) #:::
      shrink(selector.classes).map(classes => selector.copy(classes = classes))
  }

  // - Rule ------------------------------------------------------------------------------------------------------------
  // -------------------------------------------------------------------------------------------------------------------
  def attributesFor(entity: Entity): Gen[Map[Id, Id]] = {
    val attrs = entity match {
      case Entity.Node  => attributes.forNodes
      case Entity.Edge  => attributes.forEdges
      case Entity.Graph => attributes.forGraphs
    }

    attrs.filter(_.nonEmpty)
  }

  implicit val arbRule: Arbitrary[Rule] = Arbitrary(for {
    selector   <- arbitrary[Selector]
    attributes <- attributesFor(selector.entity)
  } yield Rule(selector, attributes))

  implicit val shrinkRule: Shrink[Rule] = Shrink { rule =>
    shrink(rule.selector).map(selector => rule.copy(selector = selector)) #:::
      shrink(rule.attributes).filter(_.nonEmpty).map(attributes => rule.copy(attributes = attributes))
  }

  // - Stylesheet ------------------------------------------------------------------------------------------------------
  // -------------------------------------------------------------------------------------------------------------------
  implicit val arbStylesheet: Arbitrary[Stylesheet] = Arbitrary(Gen.listOf(arbitrary[Rule]).map(Stylesheet.apply _))

  implicit val shrinkStylesheet: Shrink[Stylesheet] = Shrink { sheet =>
    shrink(sheet.rules).map(rules => sheet.copy(rules = rules))
  }

  // - Graph -----------------------------------------------------------------------------------------------------------
  // -------------------------------------------------------------------------------------------------------------------
  val graph: Gen[Graph] = for {
    id       <- arbitrary[Option[Id]]
    strict   <- arbitrary[Boolean]
    directed <- arbitrary[Boolean]
    content  <- arbitrary[GraphContent]
  } yield Graph(id, strict, directed, content)

  /** Generates smaller graphs than the default `Arbitrary`.
    *
    * Some tests are expensive - mostly those that involve running the actual DOT tool. For these, it's better
    * to generate smaller graphs so that tests don't take an unreasonable amount of time.
    */
  val smallGraph: Gen[Graph] = Gen.resize(20, graph)

  implicit val arbGraph: Arbitrary[Graph] = Arbitrary(graph)

  implicit val shrinkGraph: Shrink[Graph] = Shrink { graph =>
    shrink(graph.content).map(content => graph.copy(content = content)) #:::
      shrink(graph.id).map(id => graph.copy(id = id)) #:::
      shrink(graph.strict).map(strict => graph.copy(strict = strict)) #:::
      shrink(graph.directed).map(directed => graph.copy(directed = directed))

  }

}
