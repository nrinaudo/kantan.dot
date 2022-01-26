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

/** A set of rules to apply to graphs. */
final case class Stylesheet(rules: List[Rule]) {

  private def applyToEntity(entity: Entity, attributes: Map[Id, Id], rules: List[Rule]): Map[Id, Id] = {
    val classes = Stylesheet.extractClasses(attributes)

    val matchingRules = rules.filter(_.selector.matches(entity, classes))

    val derivedAttributes = matchingRules.foldLeft(Map.empty[Id, Id]) { (attributes, rule) =>
      attributes ++ rule.attributes
    }

    // There's a reason we do not simply fold on `attributes` instead: we want attributes already present in an
    // element to override derived ones.
    // For example:
    // - graph: a[class=red color=yellow]
    // - style: node.red[color=red]
    // - output: a[class=red color=yellow]
    derivedAttributes ++ attributes
  }

  private def applyToContent(content: GraphContent, rules: List[Rule]): GraphContent = {
    val attributes = applyToEntity(Entity.Graph, content.attributes, rules)

    val statements = content.statements.map {
      case node: Node       => node.addAttributes(applyToEntity(Entity.Node, node.attributes, rules))
      case edge: Edge       => edge.addAttributes(applyToEntity(Entity.Edge, edge.attributes, rules))
      case graph: Subgraph  => graph.copy(content = applyToContent(graph.content, rules))
      case attr: Attributes => attr
    }

    GraphContent(attributes, statements)
  }

  def applyTo(graph: Graph): Graph = {
    // All rules that apply to the document, sorted from the least to the most specific.
    val nonEmptyRules = rules
      .filter(_.attributes.nonEmpty)
      .sortBy(_.selector)

    // Rules, split depending on whether they apply to all elements of a given type, or to specific elements.
    val (generalRules, specificRules) = nonEmptyRules.partition(_.selector.classes.isEmpty)

    // Content that can be derived from general rules.
    val generalContent = generalRules.foldLeft(GraphContent.empty) {
      case (acc, Rule(Selector(Entity.Edge, _), attributes)) =>
        acc + Attributes.Edge(attributes)

      case (acc, Rule(Selector(Entity.Node, _), attributes)) =>
        acc + Attributes.Node(attributes)

      case (acc, Rule(Selector(Entity.Graph, _), attributes)) =>
        acc ++ attributes
    }

    // Content that can be derived from specific rules.
    val specificContent = applyToContent(graph.content, specificRules)

    // General statements must be declared first, to make sure they apply to all subsequent nodes, edges and subgraphs.
    // Specific attributes must override general ones to respect the "from the least to the most specific" rule.
    graph.copy(content = generalContent ++ specificContent)
  }
}

object Stylesheet {
  val empty: Stylesheet = Stylesheet(List.empty)

  def apply(rules: Rule*): Stylesheet = Stylesheet(rules.toList)

  /** Extracts the classes declared in the specified set of attributes. */
  def extractClasses(attributes: Map[Id, Id]): Set[String] = {
    val builder = Set.newBuilder[String]

    attributes.foreach {
      case (Id.Text(key), Id.Text(value)) if key == "class" =>
        builder ++= value.split(",").filter(_.nonEmpty)
      case _ => // Ignores non-textual values, because I'm not really sure what to do there...
    }

    builder.result()
  }
}

/** A rule applies a set of attributes to all elements that match its selector. */
final case class Rule(selector: Selector, attributes: Map[Id, Id])

/** A set of criterias used to select a given set of elements in a graph. */
final case class Selector(entity: Entity, classes: Set[String]) {
  def matches(entity: Entity, classes: Set[String]): Boolean =
    this.entity == entity && this.classes.subsetOf(classes)
}

object Selector {
  def general(entity: Entity): Selector = Selector(entity, Set.empty)

  implicit val ordering: Ordering[Selector] = {
    implicitly[Ordering[Int]].on(_.classes.size)
  }
}

/** Describes the various possibly types of elements. */
sealed trait Entity extends Product with Serializable

object Entity {
  final case object Node  extends Entity
  final case object Edge  extends Entity
  final case object Graph extends Entity
}
