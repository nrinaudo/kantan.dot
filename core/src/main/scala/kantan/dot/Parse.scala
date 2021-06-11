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

import _root_.fastparse.{parse => fparse, P, Parsed}
import java.io.{ByteArrayInputStream, File, FileInputStream, InputStream}

/** Abstracts over the notion of "things that can be parsed from a stream".
  *
  * The main purpose of `Parse` is to provide a common interface for parsing graph, stylesheets, and the elements
  * that compose them.
  *
  * You should in theory not need to manipulate instances of `Parse` directly, but rely on the companion object's
  * various `parse` methods instead.
  *
  * I would ideally also abstract over the notion of "things that you can read from" (eg files or streams or...), but
  * this is already part of kantan.codecs and I don't have the courage to extract that to a separate library just now.
  */
trait Parse[A] {
  def parse(in: InputStream): Either[String, A]
}

object Parse {
  def apply[A](implicit p: Parse[A]): Parse[A] = p

  // - Helper methods --------------------------------------------------------------------------------------------------
  // -------------------------------------------------------------------------------------------------------------------
  def parse[A: Parse](in: InputStream): Either[String, A] = Parse[A].parse(in)
  def parse[A: Parse](in: String): Either[String, A]      = parse[A](new ByteArrayInputStream(in.getBytes("UTF-8")))
  def parse[A: Parse](in: File): Either[String, A] = {
    val stream = new FileInputStream(in)

    try {
      parse[A](stream)
    }
    finally {
      stream.close()
    }
  }

  // - Instance creation -----------------------------------------------------------------------------------------------
  // -------------------------------------------------------------------------------------------------------------------
  def fromFastParse[A](parser: P[_] => P[A]): Parse[A] =
    in =>
      fparse(in, parser(_), true) match {
        case Parsed.Success(a, _)        => Right(a)
        case Parsed.Failure(label, _, _) => Left(label)
      }

  // - Default instances -----------------------------------------------------------------------------------------------
  // -------------------------------------------------------------------------------------------------------------------
  implicit val parseGraph: Parse[Graph]           = fromFastParse(fastparse.graph(_))
  implicit val parseStylesheet: Parse[Stylesheet] = fromFastParse(fastparse.stylesheet(_))
}
