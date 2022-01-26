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

import kantan.dot.Id
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen
import scala.io.{Codec, Source}

object attributes {
  object values {
    val escString: Gen[String] = Gen
      .listOf(
        Gen.frequency(
          1 -> attributeValues("escString"),
          8 -> Gen.alphaNumChar.map(_.toString),
          1 -> arbitrary[Char].map(_.toString)
        )
      )
      .map(_.mkString)

    val string: Gen[String]      = atom
    val double: Gen[String]      = arbitrary[Double].map(_.toString)
    val float: Gen[String]       = arbitrary[Float].map(_.toString)
    val bool: Gen[String]        = arbitrary[Boolean].map(_.toString)
    val int: Gen[String]         = arbitrary[Int].map(_.toString)
    val arrowType: Gen[String]   = attributeValues("arrowType")
    val dirType: Gen[String]     = attributeValues("dirType")
    val layout: Gen[String]      = attributeValues("layout")
    val outputMode: Gen[String]  = attributeValues("outputMode")
    val pagedir: Gen[String]     = attributeValues("pagedir")
    val rankdir: Gen[String]     = attributeValues("rankdir")
    val shape: Gen[String]       = attributeValues("shape")
    val style: Gen[String]       = attributeValues("style")
    val clusterMode: Gen[String] = attributeValues("clusterMode")
    val ordering: Gen[String]    = attributeValues("ordering")
    val packMode: Gen[String]    = attributeValues("packMode")
    val quadType: Gen[String]    = attributeValues("quadType")
    val rankType: Gen[String]    = attributeValues("rankType")
    val smoothType: Gen[String]  = attributeValues("smoothType")
    val portPos: Gen[String]     = attributeValues("portPos")
    val addDouble: Gen[String]   = addValue(double)
    val doubleList: Gen[String]  = listValue(double, ":")
    val lblString: Gen[Id]       = kantan.dot.scalacheck.id

    val color: Gen[String] = {
      val name = attributeValues("colorName")
      val rgb  = Gen.listOfN(6, Gen.hexChar).map(_.mkString)
      val rgba = Gen.listOfN(8, Gen.hexChar).map(_.mkString)

      Gen.oneOf(name, rgb, rgba)
    }

    val viewPort: Gen[String] = for {
      width  <- double
      height <- double
      zoom   <- double
      focusX <- double
      focusY <- double
    } yield s"$width,$height,$zoom,$focusX,$focusY"

    val startType: Gen[String] = for {
      style <- Gen.option(Gen.oneOf("regular", "self", "random"))
      seed  <- Gen.option(Gen.frequency(1 -> Gen.const("random"), 4 -> int))
    } yield style.getOrElse("") ++ seed.getOrElse("")

    val rect: Gen[String] = {
      def corner(default: String) = Gen.frequency(4 -> float, 1 -> Gen.const(default))

      for {
        llx <- corner("llx")
        lly <- corner("lly")
        urx <- corner("urx")
        ury <- corner("ury")
      } yield s"$llx,$lly,$urx,$ury"
    }

    val point: Gen[String] = for {
      x    <- float
      y    <- float
      tail <- Gen.frequency(4 -> "", 1 -> "!")
    } yield s"$x,$y$tail"

    val addPoint: Gen[String]  = addValue(point)
    val pointList: Gen[String] = listValue(point, " ")

    private def attributeValues(name: String): Gen[String] = Gen.oneOf(
      Source
        .fromResource(s"attributes/values/$name.txt")(Codec.UTF8)
        .getLines()
        .filter(_.nonEmpty)
        .filter(line => !line.trim().startsWith("//"))
        .toSeq
    )

    def addValue(gen: Gen[String]): Gen[String] =
      for {
        head  <- Gen.oneOf("+", "")
        value <- gen
      } yield s"$head$value"

    def listValue(gen: Gen[String], sep: String): Gen[String] =
      Gen.sized(size => Gen.resize(size / 10, Gen.listOf(gen))).map(_.mkString(sep))
  }

  val _background        = attribute("_background", values.string)
  val area               = attribute("area", values.double)
  val arrowhead          = attribute("arrowhead", values.arrowType)
  val arrowsize          = attribute("arrowsize", values.double)
  val arrowtail          = attribute("arrowtail", values.arrowType)
  val bb                 = attribute("bb", values.rect)
  val bgcolor            = attribute("bgcolor", values.color)
  val center             = attribute("center", values.bool)
  val charset            = attribute("charset", values.string)
  val cclass             = attribute("class", values.string)
  val clusterrank        = attribute("clusterrank", values.clusterMode)
  val color              = attribute("color", values.color)
  val colorscheme        = attribute("colorscheme", values.string)
  val comment            = attribute("comment", values.string)
  val compound           = attribute("compound", values.bool)
  val concentrate        = attribute("concentrate", values.bool)
  val constraint         = attribute("constraint", values.bool)
  val Damping            = attribute("Damping", values.double)
  val decorate           = attribute("decorate", values.bool)
  val defaultdist        = attribute("defaultdist", values.double)
  val dim                = attribute("dim", values.int)
  val dimen              = attribute("dimen", values.int)
  val dir                = attribute("dir", values.dirType)
  val diredgeconstraints = attribute("diredgeconstraints", values.string)
  val distortion         = attribute("distortion", values.double)
  val dpi                = attribute("dpi", values.double)
  val edgehref           = attribute("edgehref", values.escString)
  val edgetarget         = attribute("edgetarget", values.escString)
  val edgetooltip        = attribute("edgetooltip", values.escString)
  val edgeURL            = attribute("edgeURL", values.escString)
  val epsilon            = attribute("epsilon", values.double)
  val esep               = attribute("esep", values.addDouble, values.addPoint)
  val fillcolor          = attribute("fillcolor", values.color)
  val fixedsize          = attribute("fixedsize", values.bool)
  val fontcolor          = attribute("fontcolor", values.color)
  val fontname           = attribute("fontname", values.string)
  val fontnames          = attribute("fontnames", values.string)
  val fontpath           = attribute("fontpath", values.string)
  val fontsize           = attribute("fontsize", values.double)
  val forcelabels        = attribute("forcelabels", values.bool)
  val gradientangle      = attribute("gradientangle", values.int)
  val group              = attribute("group", values.string)
  val head_lp            = attribute("head_lp", values.point)
  val headclip           = attribute("headclip", values.bool)
  val headhref           = attribute("headhref", values.escString)
  val headlabel          = attributeFromId("headlabel", values.lblString)
  val headport           = attribute("headport", values.portPos)
  val headtarget         = attribute("headtarget", values.escString)
  val headtooltip        = attribute("headtooltip", values.escString)
  val headURL            = attribute("headURL", values.escString)
  val height             = attribute("height", values.double)
  val href               = attribute("href", values.escString)
  val id                 = attribute("id", values.escString)
  val image              = attribute("image", values.string)
  val imagepath          = attribute("imagepath", values.string)
  val imagepos           = attribute("imagepos", values.string)
  val imagescale         = attribute("imagescale", values.bool)
  val inputscale         = attribute("inputscale", values.double)
  val K                  = attribute("K", values.double)
  val label              = attributeFromId("label", values.lblString)
  val labelfontsize      = attribute("labelfontsize", values.double)
  val labelhref          = attribute("labelhref", values.escString)
  val labeljust          = attribute("labeljust", values.string)
  val labelloc           = attribute("labelloc", values.string)
  val layout             = attribute("layout", values.layout)
  val len                = attribute("len", values.double)
  val levels             = attribute("levels", values.int)
  val levelsgap          = attribute("levelsgap", values.double)
  val lhead              = attribute("lhead", values.string)
  val lheight            = attribute("lheight", values.double)
  val lp                 = attribute("lp", values.point)
  val ltail              = attribute("ltail", values.string)
  val lwidth             = attribute("lwidth", values.double)
  val margin             = attribute("margin", values.double)
  val maxiter            = attribute("maxiter", values.int)
  val mclimit            = attribute("mclimit", values.double)
  val mindist            = attribute("mindist", values.double)
  val minlen             = attribute("minlen", values.int)
  val mode               = attribute("mode", values.string)
  val model              = attribute("model", values.string)
  val mosek              = attribute("mosek", values.bool)
  val newrank            = attribute("newrank", values.bool)
  val nodesep            = attribute("nodesep", values.double)
  val nojustify          = attribute("nojustify", values.bool)
  val normalize          = attribute("normalize", values.double)
  val notranslate        = attribute("notranslate", values.bool)
  val nslimit            = attribute("nslimit", values.double)
  val nslimit1           = attribute("nslimit1", values.double)
  val ordering           = attribute("ordering", values.ordering)
  val orientation        = attribute("orientation", values.double)
  val outputorder        = attribute("outputorder", values.outputMode)
  val overlap            = attribute("overlap", values.string)
  val overlap_scaling    = attribute("overlap_scaling", values.double)
  val overlap_shrink     = attribute("overlap_shrink", values.bool)
  val pack               = attribute("pack", values.bool, values.int)
  val packmode           = attribute("packmode", values.packMode)
  val pad                = attribute("pad", values.double, values.point)
  val page               = attribute("page", values.double, values.point)
  val pagedir            = attribute("pagedir", values.pagedir)
  val pencolor           = attribute("pencolor", values.color)
  val penwidth           = attribute("penwidth", values.double)
  val peripheries        = attribute("peripheries", values.int)
  val pin                = attribute("pin", values.bool)
  val pos                = attribute("pos", values.point)
  val quadtree           = attribute("quadtree", values.quadType)
  val quantum            = attribute("quantum", values.double)
  val rank               = attribute("rank", values.rankType)
  val rankdir            = attribute("rankdir", values.rankdir)
  val ranksep            = attribute("ranksep", values.double, values.doubleList)
  val ratio              = attribute("ratio", values.double, values.string)
  val rects              = attribute("rects", values.rect)
  val regular            = attribute("regular", values.bool)
  val remincross         = attribute("remincross", values.bool)
  val repulsiveforce     = attribute("repulsiveforce", values.double)
  val resolution         = attribute("resolution", values.double)
  val root               = attribute("root", values.string, values.bool)
  val rotate             = attribute("rotate", values.int)
  val rotation           = attribute("rotation", values.double)
  val samehead           = attribute("samehead", values.string)
  val sametail           = attribute("sametail", values.string)
  val samplepoints       = attribute("samplepoints", values.int)
  val scale              = attribute("scale", values.double, values.point)
  val searchsize         = attribute("searchsize", values.int)
  val sep                = attribute("sep", values.addDouble, values.addPoint)
  val shape              = attribute("shape", values.shape)
  val shapefile          = attribute("shapefile", values.string)
  val showboxes          = attribute("showboxes", values.int)
  val sides              = attribute("sides", values.int)
  val size               = attribute("size", values.double, values.point)
  val skew               = attribute("skew", values.double)
  val smoothing          = attribute("smoothing", values.smoothType)
  val sortv              = attribute("sortv", values.int)
  val splines            = attribute("splines", values.bool, values.string)
  val start              = attribute("start", values.startType)
  val style              = attribute("style", values.style)
  val stylesheet         = attribute("stylesheet", values.string)
  val tail_lp            = attribute("tail_lp", values.point)
  val tailclip           = attribute("tailclip", values.bool)
  val tailhref           = attribute("tailhref", values.escString)
  val taillabel          = attributeFromId("taillabel", values.lblString)
  val tailport           = attribute("tailport", values.portPos)
  val tailtarget         = attribute("tailtarget", values.escString)
  val tailtooltip        = attribute("tailtooltip", values.escString)
  val tailURL            = attribute("tailURL", values.escString)
  val target             = attribute("target", values.escString, values.string)
  val tooltip            = attribute("tooltip", values.escString)
  val truecolor          = attribute("truecolor", values.bool)
  val URL                = attribute("URL", values.escString)
  val vertices           = attribute("vertices", values.pointList)
  val viewport           = attribute("viewport", values.viewPort)
  val voro_margin        = attribute("voro_margin", values.double)
  val weight             = attribute("weight", values.int, values.double)
  val width              = attribute("width", values.double)
  val xdotversion        = attribute("xdotversion", values.string)
  val xlabel             = attributeFromId("xlabel", values.lblString)
  val xlp                = attribute("xlp", values.point)
  val z                  = attribute("z", values.double)
  val random = for {
    name  <- atom
    value <- kantan.dot.scalacheck.id
  } yield (name, value)

  def oneOf[A](gens: Gen[A]*): Gen[A] = {
    val values = gens.toList

    Gen.choose(0, gens.size - 1).flatMap(values)
  }

  def attribute(name: String, values: Gen[String]*): Gen[(String, Id)] =
    attributeFromId(name, values.map(gen => gen.map(Id.Text)): _*)

  def attributeFromId(name: String, values: Gen[Id]*): Gen[(String, Id)] =
    oneOf(values: _*).map(value => (name, value))

  def from(gens: Gen[(String, Id)]*): Gen[Map[Id, Id]] = {
    def toIds(pair: (String, Id)): (Id, Id) = (Id.Text(pair._1), pair._2)

    Gen.sized(size => Gen.resize(size / 10, Gen.listOf(oneOf(gens: _*)))).map(_.map(toIds).toMap)
  }

  val forNodes: Gen[Map[Id, Id]] =
    from(
      area,
      cclass,
      color,
      colorscheme,
      comment,
      distortion,
      fillcolor,
      fixedsize,
      fontcolor,
      fontname,
      fontsize,
      gradientangle,
      group,
      height,
      href,
      id,
      image,
      imagepos,
      imagescale,
      label,
      labelloc,
      margin,
      nojustify,
      ordering,
      orientation,
      penwidth,
      peripheries,
      pin,
      pos,
      rects,
      regular,
      root,
      samplepoints,
      shape,
      shapefile,
      showboxes,
      sides,
      skew,
      sortv,
      style,
      target,
      tooltip,
      URL,
      vertices,
      width,
      xlabel,
      xlp,
      z,
      random
    )

  val forEdges: Gen[Map[Id, Id]] = from(
    arrowhead,
    arrowsize,
    arrowtail,
    cclass,
    color,
    colorscheme,
    comment,
    constraint,
    decorate,
    dir,
    edgehref,
    edgetarget,
    edgetooltip,
    edgeURL,
    fillcolor,
    fontcolor,
    fontname,
    fontsize,
    head_lp,
    headclip,
    headhref,
    headlabel,
    headport,
    headtarget,
    headtooltip,
    headURL,
    href,
    id,
    label,
    labelfontsize,
    labelhref,
    len,
    lhead,
    lp,
    ltail,
    minlen,
    nojustify,
    penwidth,
    pos,
    samehead,
    sametail,
    showboxes,
    style,
    tail_lp,
    tailclip,
    tailhref,
    taillabel,
    tailport,
    tailtarget,
    tailtooltip,
    tailURL,
    target,
    tooltip,
    URL,
    weight,
    xlabel,
    xlp,
    random
  )

  val forGraphs: Gen[Map[Id, Id]] = from(
    _background,
    bb,
    bgcolor,
    center,
    charset,
    cclass,
    clusterrank,
    colorscheme,
    comment,
    compound,
    concentrate,
    Damping,
    defaultdist,
    dim,
    dimen,
    diredgeconstraints,
    dpi,
    epsilon,
    esep,
    fontcolor,
    fontname,
    fontnames,
    fontpath,
    fontsize,
    forcelabels,
    gradientangle,
    href,
    id,
    imagepath,
    inputscale,
    K,
    label,
    labeljust,
    labelloc,
    layout,
    levels,
    levelsgap,
    lheight,
    lp,
    lwidth,
    margin,
    maxiter,
    mclimit,
    mindist,
    mode,
    model,
    mosek,
    newrank,
    nodesep,
    nojustify,
    normalize,
    notranslate,
    nslimit,
    nslimit1,
    ordering,
    orientation,
    outputorder,
    overlap,
    overlap_scaling,
    overlap_shrink,
    pack,
    packmode,
    pad,
    page,
    pagedir,
    quadtree,
    quantum,
    rankdir,
    ranksep,
    ratio,
    remincross,
    repulsiveforce,
    resolution,
    root,
    rotate,
    rotation,
    scale,
    searchsize,
    sep,
    showboxes,
    size,
    smoothing,
    sortv,
    splines,
    start,
    style,
    stylesheet,
    target,
    truecolor,
    URL,
    viewport,
    voro_margin,
    xdotversion,
    random
  )
}
