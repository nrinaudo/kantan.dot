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

import _root_.fastparse.JavaWhitespace._
import _root_.fastparse._

@SuppressWarnings(
  Array(
    "org.wartremover.warts.Var",
    "org.wartremover.warts.Null",
    "org.wartremover.warts.While",
    "org.wartremover.warts.StringPlusAny"
  )
)
/** Defines all parsers for DOT and DSS files.
  *
  * There should be very little reason to interact with this directly, as it's abstracted over in the more convenient
  * `Parse`.
  */
object fastparse {
  // - Atoms -----------------------------------------------------------------------------------------------------------
  // -------------------------------------------------------------------------------------------------------------------
  /** Reserved words that cannot be an unquoted atom. */
  val keywords: Set[String] = Set("graph", "digraph", "subgraph", "edge", "node")

  /** Valid identifier: starts with a letter or underscore, followed by any numbers of letters, underscores or digits. */
  def identifier[_: P]: P[String] = {

    def isIdentifierStart(c: Char) = {
      def in(min: Int, max: Int) = c >= min && c <= max

      !(in(0, 64) || in(91, 94) || c == 96 || in(123, 127) || c.isSurrogate)
    }

    def isIdentifierChar(c: Char) = isIdentifierStart(c) || (c >= 48 && c <= 57)

    (CharPred(isIdentifierStart) ~~ CharsWhile(isIdentifierChar).repX).!
  }

  /** Alpha-numeric atom (valid identifier that's not a reserved keyword). */
  def alphaNum[_: P]: P[String] = {
    def isNotKeyword(str: String) = !keywords.contains(str)

    identifier.filter(isNotKeyword)
  }

  /** Numeric atom. */
  def num[_: P]: P[String] = {
    def decimalPart  = ("." ~~ CharsWhile(_.isDigit).repX(1))
    def integralPart = CharsWhile(_.isDigit).repX(1)
    (P("-").? ~~ (decimalPart | (integralPart ~~ decimalPart.?))).!
  }

  /** Quoted atom. */
  def quoted[_: P]: P[String] = {
    def unquotedChar = CharPred(_ != '"').!.map(_.toString)

    def quotedChar = {
      def quote     = P("\\\"").map(_ => "\"")
      def lineBreak = P("\\\n").map(_ => "")
      def other     = ("\\" ~ unquotedChar).!

      quote | lineBreak | other
    }

    def quotedContent =
      ((quotedChar | unquotedChar).repX).map(_.mkString)

    (P("\"") ~~ quotedContent ~~ P("\"")).rep(min = 1, sep = "+").map(_.mkString)
  }

  def html[_: P]: P[Id.Html] = {
    def raw = CharsWhile(c => c != '<' && c != '>').!.map(_.toString)
    def tag = P("<") ~~ raw.map(s => s"<$s>") ~~ P(">")

    P("<") ~~ (raw | tag).repX.map(ss => Id.Html(ss.mkString)) ~~ P(">")
  }

  def string[_: P]: P[Id.Text] =
    (alphaNum | num | quoted).map(Id.Text.apply)

  /** Any atom. */
  def atom[_: P]: P[Id] =
    string | html

  // - Attributes ------------------------------------------------------------------------------------------------------
  // -------------------------------------------------------------------------------------------------------------------
  /** Any sequence of attributes between brackets.
    *
    * The DOT specifications allow multiple sequences of brackets, but since that has exactly the same meaning as
    * a single one, we just collapse everything into a single `Map[String, String]`.
    */
  def attributes[_: P]: P[Map[Id, Id]] = {
    def list = ("[" ~ (atom ~ "=" ~ atom ~ (";" | ",").?).rep ~ "]")

    list.rep(1).map(_.foldLeft(Map.empty[Id, Id])(_ ++ _.toMap))
  }

  /** Optional attributes: if absent, will yield the empty set of attributes. */
  def optAttributes[_: P]: P[Map[Id, Id]] = attributes.?.map(_.getOrElse(Map.empty))

  /** Attributes statement
    *
    * This, and all "statement" parsers, yield a `GraphContent`: a DOT statement can translate into any number of
    * statements in our internal representation, as well as attributes for the containing graph.
    */
  def attributesStmt[_: P]: P[GraphContent] = {
    def node = P("node").map(_ => Attributes.Node.apply _)
    def edge = P("edge").map(_ => Attributes.Edge.apply _)

    ((node | edge) ~ attributes).map {
      case (create, attrs) => GraphContent(List(create(attrs)))
    }
  }

  // - Node ------------------------------------------------------------------------------------------------------------
  // -------------------------------------------------------------------------------------------------------------------
  /** Single node identifier.
    *
    * Note that this should never be used directly: DOT grammar always allows you to specify a list of node ids. Prefer
    * `nodeIds`.
    */
  def nodeId[_: P]: P[NodeId] = {
    def reference = (":" ~ atom ~ ":" ~ atom).map {
      case (id, point) => Port.Compound(id, point)
    }

    def point =
      (":" ~ atom)
        .map(Port.Simple)

    def port =
      (reference | point).?.map(_.getOrElse(Port.None))

    (atom ~ port).map { case (id, port) => NodeId(id, port) }
  }

  /** List of node identifiers. */
  def nodeIds[_: P]: P[List[NodeId]] =
    nodeId.rep(min = 1, sep = ",").map(_.toList)

  /** Nodes statement
    *
    * This, and all "statement" parsers, yield a `GraphContent`: a DOT statement can translate into any number of
    * statements in our internal representation, as well as attributes for the containing graph.
    */
  def nodeStmt[_: P]: P[GraphContent] =
    (nodeIds ~ optAttributes).map {
      case (ids, attrs) =>
        GraphContent(ids.map(id => Node(id.id, attrs)))
    }

  // - Subgraph --------------------------------------------------------------------------------------------------------
  // -------------------------------------------------------------------------------------------------------------------
  /** A single subgraph. */
  def subgraph[_: P]: P[Subgraph] = {
    def graphId = ("subgraph" ~ atom.?).?.map(_.flatten)

    (graphId ~ "{" ~ graphContent ~ "}").map { case (id, content) => Subgraph(id, content) }
  }

  /** Subgraphs statement
    *
    * This, and all "statement" parsers, yield a `GraphContent`: a DOT statement can translate into any number of
    * statements in our internal representation, as well as attributes for the containing graph.
    */
  def subgraphStmt[_: P]: P[GraphContent] = subgraph.map { s =>
    GraphContent(List(s))
  }

  // - Edge ------------------------------------------------------------------------------------------------------------
  // -------------------------------------------------------------------------------------------------------------------
  // This is a little bit tricky, because I didn't want to have complex representation of edges.
  // DOT allows us to have lists of nodes, or even subgraphs, as extremities of an edge.
  // For example:
  // a, b -> subgraph {c, d}
  //
  // This is syntactic sugar for:
  // a, b
  // subgraph {c, d}
  // a -> c
  // a -> d
  // b -> c
  // b -> d
  //
  // In order to make our internal representation simpler, all these edges are expended at parse time, allowing us to
  // have a very simple "node ID to node ID" representation for edges.
  //
  // It does, however, make the parsing code a little bit more fiddly...

  /** Extracts all nodes found in a subgraph, so that we can do the "edge multiplexing" trick supported by DOT. */
  private def extractNodes(graph: Subgraph): List[NodeId] =
    graph.content.statements
      .foldLeft(Set.newBuilder[NodeId]) {
        // Nodes are obviously nodes.
        case (acc, Node(id, _)) => acc += NodeId(id, Port.None)

        // Edges are seen as containers for two nodes: head and tail.
        case (acc, Edge(head, tail, _)) =>
          acc += head
          acc += tail

        // Subgraph are recursive container for nodes.
        // I could have made this tail recursive, but the code would be even worse, and if anyone manages to write
        // a graph with enough nested subgraphs to blow the stack, they deserve what they get.
        case (acc, subgraph: Subgraph) => acc ++= extractNodes(subgraph)

        // Any other statement doesn't contain nodes.
        case (acc, _) => acc
      }
      .result()
      .toList

  /** Given two lists of nodes, merges them into a list of edges.
    *
    * We simply associate every node on the left to every node on the right.
    */
  private def mergeNodes(lhs: List[NodeId], rhs: List[NodeId], attrs: Map[Id, Id]): List[Edge] =
    for {
      head <- lhs
      tail <- rhs
    } yield Edge(head, tail, attrs)

  /** Edges statement
    *
    * This, and all "statement" parsers, yield a `GraphContent`: a DOT statement can translate into any number of
    * statements in our internal representation, as well as attributes for the containing graph.
    */
  def edgeStmt[_: P]: P[GraphContent] = {
    // An extremity is either a subgraph or a list of nodes.
    def extremity =
      subgraph.map(Left.apply) | nodeIds.map(Right.apply)

    // This means we're slightly more tolerant than DOT: we'll accept `--` for directed graphs, which is illegal.
    // I'm fine with being more permissive - the opposite would be problematic.
    def rest =
      ("->" | "--") ~ extremity

    (extremity ~ rest.rep(1) ~ optAttributes).map {
      case (head, tail, attrs) =>
        val list = head +: tail

        val subgraphs = list.collect {
          case Left(subgraph) => subgraph
        }

        val nodes = list.map {
          case Left(subgraph) => extractNodes(subgraph)
          case Right(nodes)   => nodes
        }

        // I always get confused by `is.zip(is.drop(1))`, so here's a note to my future self:
        // this simply creates a list of tuples associating each element in the original list with the next one.
        // For example, if `is` is `(a, b, c)`, we'll get `((a, b), (b, c), (c, d))`.
        val edges = nodes.zip(nodes.drop(1)).foldLeft(List.empty[Edge]) {
          case (acc, (lhs, rhs)) =>
            acc ++ mergeNodes(lhs, rhs, attrs)
        }

        GraphContent((subgraphs ++ edges).toList)
    }
  }

  // - Graph -----------------------------------------------------------------------------------------------------------
  // -------------------------------------------------------------------------------------------------------------------
  /** Graph attributes.
    *
    * This parses both `graph[foo=bar]` and `foo=bar` statements and wraps their in their internal representation,
    * `Map[String, String]`.
    */
  def graphAttributes[_: P]: P[GraphContent] = {
    def single   = (atom ~ "=" ~ atom).map { case (key, value) => Map(key -> value) }
    def multiple = "graph" ~ attributes

    (single | multiple).map(GraphContent.apply)
  }

  /** Content of a graph.
    *
    * All statements are expended and turned into our internal representation of a graph. The resulting structure
    * should be equivalent, but if you're seeing weird behaviours in real life, here's a good place to start looking
    * for bugs.
    */
  def graphContent[_: P]: P[GraphContent] = {
    def statement: P[GraphContent] =
      graphAttributes | attributesStmt | edgeStmt | subgraphStmt | nodeStmt

    (statement ~ ";".?).rep
      .map(_.foldLeft(GraphContent.empty)(_ ++ _))
  }

  /** Parses a graph declaration. */
  def graph[_: P]: P[Graph] = {
    def strict = P("strict").!.?.map {
      case Some(_) => true
      case None    => false
    }

    def unigraph =
      P("graph").map(_ => false)

    def digraph =
      P("digraph").map(_ => true)

    (Start ~ strict ~ (unigraph | digraph) ~ atom.? ~ "{" ~ graphContent ~ "}" ~ End).map {
      case (strict, directed, id, body) => Graph(id, strict, directed, body)
    }
  }

  // - Stylesheet selector ---------------------------------------------------------------------------------------------
  // -------------------------------------------------------------------------------------------------------------------
  def entity[_: P]: P[Entity] = {
    def graph = P("graph").map(_ => Entity.Graph)
    def node  = P("node").map(_ => Entity.Node)
    def edge  = P("edge").map(_ => Entity.Edge)

    graph | node | edge
  }

  def selectors[_: P]: P[List[Selector]] = {

    def classes =
      ("." ~~ identifier).rep(1).map(_.toSet)

    def withEntity = (entity ~~ classes.?).map { case (e, c) => List(Selector(e, c.getOrElse(Set.empty))) }

    // Selectors without entities are mapped to one selector per existing entity. Not the most compact representation,
    // but it does make things a lot simpler down the road.
    def noEntity =
      classes.map(cls =>
        List(
          Selector(Entity.Node, cls),
          Selector(Entity.Edge, cls),
          Selector(Entity.Graph, cls)
        )
      )

    (withEntity | noEntity).rep(sep = ",", min = 1).map(_.toList.flatten)
  }

  // - Stylesheet rules ------------------------------------------------------------------------------------------------
  // -------------------------------------------------------------------------------------------------------------------

  def rule[_: P]: P[List[Rule]] = {
    def content = (atom ~ ":" ~ atom ~ ";".?).rep.map(_.toMap)

    (selectors ~ "{" ~ content ~ "}").map {
      case (selectors, attributes) => selectors.map(selector => Rule(selector, attributes))
    }
  }

  def rules[_: P]: P[List[Rule]] = rule.rep.map(_.toList.flatten)

  // - Stylesheet ------------------------------------------------------------------------------------------------------
  // -------------------------------------------------------------------------------------------------------------------
  def stylesheet[_: P]: P[Stylesheet] =
    Start ~ rules.map(Stylesheet.apply _) ~ End
}
