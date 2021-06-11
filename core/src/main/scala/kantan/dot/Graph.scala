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

/** Root of a diagram. */
final case class Graph(id: Option[String], strict: Boolean, directed: Boolean, content: GraphContent)

/** Content of graphs and subgraphs.
  *
  * The `attributes` part might be a little surprising - this doesn't work the way I assumed at first: graph attributes
  * can be specified with two different kinds of statements:
  * {{{
  * graph {
  *     color=white
  *     graph[bgcolor=red]
  * }
  * }}}
  *
  * These are aggregated into a single `Map[String, String]` containing the final value of each attribute. Note that
  * this is a lossy transformation (we can't get back to the original input) but one that has no impact on the final
  * diagram, as we only lose syntax, not semantics.
  */
final case class GraphContent(attributes: Map[String, String], statements: List[Statement]) {
  @SuppressWarnings(Array("org.wartremover.warts.ListAppend"))
  def +(statement: Statement): GraphContent =
    copy(statements = statements :+ statement)

  def ++(attrs: Map[String, String]): GraphContent =
    copy(attributes = attributes ++ attrs)

  def ++(stmts: List[Statement]): GraphContent =
    copy(statements = statements ++ stmts)

  def ++(other: GraphContent): GraphContent = copy(
    attributes = attributes ++ other.attributes,
    statements = statements ++ other.statements
  )
}

object GraphContent {
  val empty: GraphContent = GraphContent(Map.empty, List.empty)

  def apply(statements: List[Statement]): GraphContent     = GraphContent(Map.empty, statements)
  def apply(attributes: Map[String, String]): GraphContent = GraphContent(attributes, List.empty)
}

/** Statement that can be found inside of a graph / subgraph. */
sealed abstract class Statement extends Product with Serializable

/** General attribute statements about either nodes or edges. */
sealed abstract class Attributes extends Statement

object Attributes {

  /** Attributes that apply to all nodes in the graph and its descendants. */
  final case class Node(value: Map[String, String]) extends Attributes

  /** Attributes that apply to all edges in the graph and its descendants. */
  final case class Edge(value: Map[String, String]) extends Attributes
}

/** Things that have a visual representation (nodes, edges or subgraphs). */
sealed abstract class Element extends Statement {
  def attributes: Map[String, String]
}

/** A point in a graph. */
final case class Node(id: String, attributes: Map[String, String]) extends Element {
  def addAttributes(attrs: Map[String, String]): Node = copy(attributes = attributes ++ attrs)
}

/** A link between two nodes. */
final case class Edge(head: NodeId, tail: NodeId, attributes: Map[String, String]) extends Element {
  def addAttributes(attrs: Map[String, String]): Edge = copy(attributes = attributes ++ attrs)
}

/** A nested graph. */
final case class Subgraph(id: Option[String], content: GraphContent) extends Element {
  override def attributes = content.attributes
}

/** Unique node identifier. */
final case class NodeId(id: String, port: Port)

object NodeId {
  def apply(id: String): NodeId = NodeId(id, Port.None)
}

/** Port part of a node identifier.
  *
  * This is, for example, how you can specify the source of an edge.
  */
sealed abstract class Port extends Product with Serializable
object Port {
  final case object None                               extends Port
  final case class Simple(id: String)                  extends Port
  final case class Compound(id: String, point: String) extends Port
}
