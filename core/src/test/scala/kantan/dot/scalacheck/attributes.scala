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
    val lblString: Gen[String]   = escString // We do not support HTML labels

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

  val _background: Gen[(String, String)]        = attribute("_background", values.string)
  val area: Gen[(String, String)]               = attribute("area", values.double)
  val arrowhead: Gen[(String, String)]          = attribute("arrowhead", values.arrowType)
  val arrowsize: Gen[(String, String)]          = attribute("arrowsize", values.double)
  val arrowtail: Gen[(String, String)]          = attribute("arrowtail", values.arrowType)
  val bb: Gen[(String, String)]                 = attribute("bb", values.rect)
  val bgcolor: Gen[(String, String)]            = attribute("bgcolor", values.color)
  val center: Gen[(String, String)]             = attribute("center", values.bool)
  val charset: Gen[(String, String)]            = attribute("charset", values.string)
  val cclass: Gen[(String, String)]             = attribute("class", values.string)
  val clusterrank: Gen[(String, String)]        = attribute("clusterrank", values.clusterMode)
  val color: Gen[(String, String)]              = attribute("color", values.color)
  val colorscheme: Gen[(String, String)]        = attribute("colorscheme", values.string)
  val comment: Gen[(String, String)]            = attribute("comment", values.string)
  val compound: Gen[(String, String)]           = attribute("compound", values.bool)
  val concentrate: Gen[(String, String)]        = attribute("concentrate", values.bool)
  val constraint: Gen[(String, String)]         = attribute("constraint", values.bool)
  val Damping: Gen[(String, String)]            = attribute("Damping", values.double)
  val decorate: Gen[(String, String)]           = attribute("decorate", values.bool)
  val defaultdist: Gen[(String, String)]        = attribute("defaultdist", values.double)
  val dim: Gen[(String, String)]                = attribute("dim", values.int)
  val dimen: Gen[(String, String)]              = attribute("dimen", values.int)
  val dir: Gen[(String, String)]                = attribute("dir", values.dirType)
  val diredgeconstraints: Gen[(String, String)] = attribute("diredgeconstraints", values.string)
  val distortion: Gen[(String, String)]         = attribute("distortion", values.double)
  val dpi: Gen[(String, String)]                = attribute("dpi", values.double)
  val edgehref: Gen[(String, String)]           = attribute("edgehref", values.escString)
  val edgetarget: Gen[(String, String)]         = attribute("edgetarget", values.escString)
  val edgetooltip: Gen[(String, String)]        = attribute("edgetooltip", values.escString)
  val edgeURL: Gen[(String, String)]            = attribute("edgeURL", values.escString)
  val epsilon: Gen[(String, String)]            = attribute("epsilon", values.double)
  val esep: Gen[(String, String)]               = attribute("esep", values.addDouble, values.addPoint)
  val fillcolor: Gen[(String, String)]          = attribute("fillcolor", values.color)
  val fixedsize: Gen[(String, String)]          = attribute("fixedsize", values.bool)
  val fontcolor: Gen[(String, String)]          = attribute("fontcolor", values.color)
  val fontname: Gen[(String, String)]           = attribute("fontname", values.string)
  val fontnames: Gen[(String, String)]          = attribute("fontnames", values.string)
  val fontpath: Gen[(String, String)]           = attribute("fontpath", values.string)
  val fontsize: Gen[(String, String)]           = attribute("fontsize", values.double)
  val forcelabels: Gen[(String, String)]        = attribute("forcelabels", values.bool)
  val gradientangle: Gen[(String, String)]      = attribute("gradientangle", values.int)
  val group: Gen[(String, String)]              = attribute("group", values.string)
  val head_lp: Gen[(String, String)]            = attribute("head_lp", values.point)
  val headclip: Gen[(String, String)]           = attribute("headclip", values.bool)
  val headhref: Gen[(String, String)]           = attribute("headhref", values.escString)
  val headlabel: Gen[(String, String)]          = attribute("headlabel", values.lblString)
  val headport: Gen[(String, String)]           = attribute("headport", values.portPos)
  val headtarget: Gen[(String, String)]         = attribute("headtarget", values.escString)
  val headtooltip: Gen[(String, String)]        = attribute("headtooltip", values.escString)
  val headURL: Gen[(String, String)]            = attribute("headURL", values.escString)
  val height: Gen[(String, String)]             = attribute("height", values.double)
  val href: Gen[(String, String)]               = attribute("href", values.escString)
  val id: Gen[(String, String)]                 = attribute("id", values.escString)
  val image: Gen[(String, String)]              = attribute("image", values.string)
  val imagepath: Gen[(String, String)]          = attribute("imagepath", values.string)
  val imagepos: Gen[(String, String)]           = attribute("imagepos", values.string)
  val imagescale: Gen[(String, String)]         = attribute("imagescale", values.bool)
  val inputscale: Gen[(String, String)]         = attribute("inputscale", values.double)
  val K: Gen[(String, String)]                  = attribute("K", values.double)
  val label: Gen[(String, String)]              = attribute("label", values.lblString)
  val labelfontsize: Gen[(String, String)]      = attribute("labelfontsize", values.double)
  val labelhref: Gen[(String, String)]          = attribute("labelhref", values.escString)
  val labeljust: Gen[(String, String)]          = attribute("labeljust", values.string)
  val labelloc: Gen[(String, String)]           = attribute("labelloc", values.string)
  val layout: Gen[(String, String)]             = attribute("layout", values.layout)
  val len: Gen[(String, String)]                = attribute("len", values.double)
  val levels: Gen[(String, String)]             = attribute("levels", values.int)
  val levelsgap: Gen[(String, String)]          = attribute("levelsgap", values.double)
  val lhead: Gen[(String, String)]              = attribute("lhead", values.string)
  val lheight: Gen[(String, String)]            = attribute("lheight", values.double)
  val lp: Gen[(String, String)]                 = attribute("lp", values.point)
  val ltail: Gen[(String, String)]              = attribute("ltail", values.string)
  val lwidth: Gen[(String, String)]             = attribute("lwidth", values.double)
  val margin: Gen[(String, String)]             = attribute("margin", values.double)
  val maxiter: Gen[(String, String)]            = attribute("maxiter", values.int)
  val mclimit: Gen[(String, String)]            = attribute("mclimit", values.double)
  val mindist: Gen[(String, String)]            = attribute("mindist", values.double)
  val minlen: Gen[(String, String)]             = attribute("minlen", values.int)
  val mode: Gen[(String, String)]               = attribute("mode", values.string)
  val model: Gen[(String, String)]              = attribute("model", values.string)
  val mosek: Gen[(String, String)]              = attribute("mosek", values.bool)
  val newrank: Gen[(String, String)]            = attribute("newrank", values.bool)
  val nodesep: Gen[(String, String)]            = attribute("nodesep", values.double)
  val nojustify: Gen[(String, String)]          = attribute("nojustify", values.bool)
  val normalize: Gen[(String, String)]          = attribute("normalize", values.double)
  val notranslate: Gen[(String, String)]        = attribute("notranslate", values.bool)
  val nslimit: Gen[(String, String)]            = attribute("nslimit", values.double)
  val nslimit1: Gen[(String, String)]           = attribute("nslimit1", values.double)
  val ordering: Gen[(String, String)]           = attribute("ordering", values.ordering)
  val orientation: Gen[(String, String)]        = attribute("orientation", values.double)
  val outputorder: Gen[(String, String)]        = attribute("outputorder", values.outputMode)
  val overlap: Gen[(String, String)]            = attribute("overlap", values.string)
  val overlap_scaling: Gen[(String, String)]    = attribute("overlap_scaling", values.double)
  val overlap_shrink: Gen[(String, String)]     = attribute("overlap_shrink", values.bool)
  val pack: Gen[(String, String)]               = attribute("pack", values.bool, values.int)
  val packmode: Gen[(String, String)]           = attribute("packmode", values.packMode)
  val pad: Gen[(String, String)]                = attribute("pad", values.double, values.point)
  val page: Gen[(String, String)]               = attribute("page", values.double, values.point)
  val pagedir: Gen[(String, String)]            = attribute("pagedir", values.pagedir)
  val pencolor: Gen[(String, String)]           = attribute("pencolor", values.color)
  val penwidth: Gen[(String, String)]           = attribute("penwidth", values.double)
  val peripheries: Gen[(String, String)]        = attribute("peripheries", values.int)
  val pin: Gen[(String, String)]                = attribute("pin", values.bool)
  val pos: Gen[(String, String)]                = attribute("pos", values.point)
  val quadtree: Gen[(String, String)]           = attribute("quadtree", values.quadType)
  val quantum: Gen[(String, String)]            = attribute("quantum", values.double)
  val rank: Gen[(String, String)]               = attribute("rank", values.rankType)
  val rankdir: Gen[(String, String)]            = attribute("rankdir", values.rankdir)
  val ranksep: Gen[(String, String)]            = attribute("ranksep", values.double, values.doubleList)
  val ratio: Gen[(String, String)]              = attribute("ratio", values.double, values.string)
  val rects: Gen[(String, String)]              = attribute("rects", values.rect)
  val regular: Gen[(String, String)]            = attribute("regular", values.bool)
  val remincross: Gen[(String, String)]         = attribute("remincross", values.bool)
  val repulsiveforce: Gen[(String, String)]     = attribute("repulsiveforce", values.double)
  val resolution: Gen[(String, String)]         = attribute("resolution", values.double)
  val root: Gen[(String, String)]               = attribute("root", values.string, values.bool)
  val rotate: Gen[(String, String)]             = attribute("rotate", values.int)
  val rotation: Gen[(String, String)]           = attribute("rotation", values.double)
  val samehead: Gen[(String, String)]           = attribute("samehead", values.string)
  val sametail: Gen[(String, String)]           = attribute("sametail", values.string)
  val samplepoints: Gen[(String, String)]       = attribute("samplepoints", values.int)
  val scale: Gen[(String, String)]              = attribute("scale", values.double, values.point)
  val searchsize: Gen[(String, String)]         = attribute("searchsize", values.int)
  val sep: Gen[(String, String)]                = attribute("sep", values.addDouble, values.addPoint)
  val shape: Gen[(String, String)]              = attribute("shape", values.shape)
  val shapefile: Gen[(String, String)]          = attribute("shapefile", values.string)
  val showboxes: Gen[(String, String)]          = attribute("showboxes", values.int)
  val sides: Gen[(String, String)]              = attribute("sides", values.int)
  val size: Gen[(String, String)]               = attribute("size", values.double, values.point)
  val skew: Gen[(String, String)]               = attribute("skew", values.double)
  val smoothing: Gen[(String, String)]          = attribute("smoothing", values.smoothType)
  val sortv: Gen[(String, String)]              = attribute("sortv", values.int)
  val splines: Gen[(String, String)]            = attribute("splines", values.bool, values.string)
  val start: Gen[(String, String)]              = attribute("start", values.startType)
  val style: Gen[(String, String)]              = attribute("style", values.style)
  val stylesheet: Gen[(String, String)]         = attribute("stylesheet", values.string)
  val tail_lp: Gen[(String, String)]            = attribute("tail_lp", values.point)
  val tailclip: Gen[(String, String)]           = attribute("tailclip", values.bool)
  val tailhref: Gen[(String, String)]           = attribute("tailhref", values.escString)
  val taillabel: Gen[(String, String)]          = attribute("taillabel", values.lblString)
  val tailport: Gen[(String, String)]           = attribute("tailport", values.portPos)
  val tailtarget: Gen[(String, String)]         = attribute("tailtarget", values.escString)
  val tailtooltip: Gen[(String, String)]        = attribute("tailtooltip", values.escString)
  val tailURL: Gen[(String, String)]            = attribute("tailURL", values.escString)
  val target: Gen[(String, String)]             = attribute("target", values.escString, values.string)
  val tooltip: Gen[(String, String)]            = attribute("tooltip", values.escString)
  val truecolor: Gen[(String, String)]          = attribute("truecolor", values.bool)
  val URL: Gen[(String, String)]                = attribute("URL", values.escString)
  val vertices: Gen[(String, String)]           = attribute("vertices", values.pointList)
  val viewport: Gen[(String, String)]           = attribute("viewport", values.viewPort)
  val voro_margin: Gen[(String, String)]        = attribute("voro_margin", values.double)
  val weight: Gen[(String, String)]             = attribute("weight", values.int, values.double)
  val width: Gen[(String, String)]              = attribute("width", values.double)
  val xdotversion: Gen[(String, String)]        = attribute("xdotversion", values.string)
  val xlabel: Gen[(String, String)]             = attribute("xlabel", values.lblString)
  val xlp: Gen[(String, String)]                = attribute("xlp", values.point)
  val z: Gen[(String, String)]                  = attribute("z", values.double)
  val random: Gen[(String, String)] = for {
    name  <- atom
    value <- atom
  } yield (name, value)

  def oneOf[A](gens: Gen[A]*): Gen[A] = {
    val values = gens.toList

    Gen.choose(0, gens.size - 1).flatMap(values)
  }

  def attribute(name: String, values: Gen[String]*): Gen[(String, String)] =
    oneOf(values: _*).map(value => (name, value))

  def from(gens: Gen[(String, String)]*): Gen[Map[String, String]] =
    Gen.sized(size => Gen.resize(size / 10, Gen.listOf(oneOf(gens: _*)))).map(_.toMap)

  val forNodes: Gen[Map[String, String]] =
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

  val forEdges: Gen[Map[String, String]] = from(
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

  val forGraphs: Gen[Map[String, String]] = from(
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
