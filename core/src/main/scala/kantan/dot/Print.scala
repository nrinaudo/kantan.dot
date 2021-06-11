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

import java.io.{BufferedWriter, File, FileOutputStream, OutputStream, OutputStreamWriter, StringWriter, Writer}

/** Abstracts over the notion of "things that can be printed".
  *
  * The main purpose of `Print` is to provide a common interface for printing graph, stylesheets, and the elements
  * that compose them.
  *
  * You should in theory not need to manipulate instances of `Print` directly, but rely on the companion object's
  * various `print` methods instead.
  *
  * I would ideally also abstract over the notion of "things that you can print to" (eg files or streams or...), but
  * this is already part of kantan.codecs and I don't have the courage to extract that to a separate library just now.
  */
trait Print[A] {

  /** Prints the specified object to the specified writer. */
  def print(a: A, out: Writer): Unit
}

object Print {
  def apply[A](implicit p: Print[A]): Print[A] = p

  // - Helper methods --------------------------------------------------------------------------------------------------
  // -------------------------------------------------------------------------------------------------------------------
  /** Prints the specified object to the specified writer. */
  def print[A: Print](a: A, out: Writer): Unit = Print[A].print(a, out)

  /** Prints the specified object to stdout. */
  def print[A: Print](a: A): Unit = print(a, System.out)

  /** Prints the specified object to the specified output stream (using UTF-8 because we're not savages). */
  def print[A: Print](a: A, out: OutputStream): Unit = {
    val writer = new BufferedWriter(new OutputStreamWriter(out, "UTF-8"))

    Print[A].print(a, writer)
    writer.flush()
  }

  /** Prints the specified object to the specified UTF-8 encoded file. */
  def print[A: Print](a: A, out: File): Unit = {
    val stream = new FileOutputStream(out)

    try {
      print[A](a, stream)
    }
    finally {
      stream.close()
    }

  }

  /** Returns the specified object as a string. */
  def toString[A: Print](a: A): String = {
    val out = new StringWriter()
    Print[A].print(a, out)
    out.toString
  }

  // - Stylesheet instances --------------------------------------------------------------------------------------------
  // -------------------------------------------------------------------------------------------------------------------
  object style {
    def printEntity(entity: Entity, out: Writer): Unit = entity match {
      case Entity.Node  => out.write("node")
      case Entity.Edge  => out.write("edge")
      case Entity.Graph => out.write("graph")
    }

    def printClasses(classes: Set[String], out: Writer): Unit =
      classes.foreach(cls => out.write(s".$cls"))

    def printSelector(selector: Selector, out: Writer): Unit = {
      printEntity(selector.entity, out)
      printClasses(selector.classes, out)
    }

    def printAttributes(attributes: Map[String, String], out: Writer): Unit = {
      out.write(" {\n")
      attributes.foreach {
        case (key, value) =>
          out.write("  ")
          graph.printAtom(key, out)
          out.write(": ")
          graph.printAtom(value, out)
          out.write(";\n")
      }
      out.write("}")
    }

    def printRule(rule: Rule, out: Writer): Unit =
      if(rule.attributes.nonEmpty) {
        printSelector(rule.selector, out)
        printAttributes(rule.attributes, out)
      }

    def printStylesheet(stylesheet: Stylesheet, out: Writer): Unit =
      stylesheet.rules.foreach { rule =>
        printRule(rule, out)
        out.write("\n")
      }
  }

  implicit val printStylesheet: Print[Stylesheet] = style.printStylesheet

  // - Graph instances -------------------------------------------------------------------------------------------------
  // -------------------------------------------------------------------------------------------------------------------
  private object graph {
    def printAtom(atom: String, out: Writer): Unit = {
      // This could be improved quite a bit (and is in fact probably a little bit buggy when it comes to escaping).
      // I'm simply quoting every single atom, which is technically correct but also not very pleasant to read. I'm
      // ok with that for the moment, as the output of kantan.dot is not meant for human consumption, but might get
      // tired of it at some point and fix it.
      out.write("\"")
      out.write(atom.replace("\"", "\\\""))
      out.write("\"")
    }

    def printAttributeStatement(name: String, attributes: Map[String, String], indent: Int, out: Writer): Unit =
      if(attributes.nonEmpty) {
        printIndent(indent, out)
        out.write(name)
        printAttributes(attributes, out)
      }

    @SuppressWarnings(Array("org.wartremover.warts.Var"))
    def printAttributes(attributes: Map[String, String], out: Writer): Unit = if(attributes.nonEmpty) {
      out.write("[")
      var isFirst = true
      attributes.foreach {
        case (key, value) =>
          if(!isFirst) out.write(" ")
          else isFirst = false
          printAtom(key, out)
          out.write("=")
          printAtom(value, out)
      }
      out.write("]")
    }

    def printNodeId(id: NodeId, out: Writer): Unit = {
      printAtom(id.id, out)
      id.port match {
        case Port.None =>
        case Port.Simple(part) =>
          out.write(":")
          printAtom(part, out)
        case Port.Compound(part1, part2) =>
          out.write(":")
          printAtom(part1, out)
          out.write(":")
          printAtom(part2, out)
      }
    }

    def printIndent(indent: Int, out: Writer): Unit =
      (0 to indent).foreach { _ =>
        out.write(" ")
      }

    def printStatement(statement: Statement, directed: Boolean, indent: Int, out: Writer): Unit = statement match {
      case Attributes.Node(attrs) => printAttributeStatement("node", attrs, indent, out)

      case Attributes.Edge(attrs) => printAttributeStatement("edge", attrs, indent, out)

      case node: Node =>
        printIndent(indent, out)
        printAtom(node.id, out)
        printAttributes(node.attributes, out)

      case edge: Edge =>
        printIndent(indent, out)
        printNodeId(edge.head, out)
        if(directed) out.write(" -> ")
        else out.write(" -- ")
        printNodeId(edge.tail, out)
        printAttributes(edge.attributes, out)

      case subgraph: Subgraph =>
        printIndent(indent, out)
        subgraph.id.foreach { id =>
          out.write("subgraph ")
          printAtom(id, out)
          out.write(" ")
        }
        out.write("{\n")
        printGraphContent(subgraph.content, directed, indent + 2, out)
        printIndent(indent, out)
        out.write("}")

    }

    def printGraphContent(content: GraphContent, directed: Boolean, indent: Int, out: Writer): Unit = {
      printAttributeStatement("graph", content.attributes, indent, out)
      out.write("\n")

      content.statements.foreach { stmt =>
        printStatement(stmt, directed, indent, out)
        out.write("\n")
      }
    }

    def printGraph(graph: Graph, out: Writer): Unit = {
      if(graph.strict) out.write("strict ")

      if(graph.directed) out.write("digraph ")
      else out.write("graph ")

      graph.id.foreach { id =>
        printAtom(id, out)
        out.write(" ")
      }
      out.write("{\n")
      printGraphContent(graph.content, graph.directed, 2, out)

      out.write("}\n")
    }
  }

  implicit val printGraph: Print[Graph] = graph.printGraph
}
