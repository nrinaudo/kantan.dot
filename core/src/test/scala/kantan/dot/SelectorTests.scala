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

import kantan.dot.scalacheck._
import org.scalacheck.{Arbitrary, Shrink}
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Shrink.shrink
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

class SelectorTests extends AnyFunSuite with Matchers with ScalaCheckDrivenPropertyChecks {
  // In the context of these tests:
  // - select XYZ: XYZ used for selection. For example, select entity is the entity we're selecting on.
  // - actual XYZ: XYZ found in the actual graph. For example, actual entity is the entity we've found in the graph.

  test("Differing select and actual entities should not match") {
    forAll { (selectEntity: Entity, actualEntity: Entity, selectClasses: Set[String], actualClasses: Set[String]) =>
      if(selectEntity != actualEntity) {
        val selector = Selector(selectEntity, selectClasses)

        selector.matches(actualEntity, actualClasses) should be(false)
      }
    }
  }

  test("Identical select and actual entities should match in the absence of classes") {
    forAll { (entity: Entity) =>
      val selector = Selector(entity, Set.empty)

      selector.matches(entity, Set.empty) should be(true)
    }
  }

  test("Identical select and actual entities should match if select classes are a subset of actual classes") {
    forAll { (entity: Entity, classes: NestedSets[String]) =>
      val NestedSets(selectClasses, actualClasses) = classes

      val selector = Selector(entity, selectClasses)

      selector.matches(entity, actualClasses) should be(true)
    }
  }

  test("Identical select and actual entities shouldn't match if select classes are a not a subset of actual classes") {
    forAll { (entity: Entity, sets: NonNestedSets[String]) =>
      val NonNestedSets(selectClasses, actualClasses) = sets

      val selector = Selector(entity, selectClasses)

      selector.matches(entity, actualClasses) should be(false)
    }
  }
}

/** Holds a pair of sets such that `subset` is always a subset of `superset`. */
final case class NestedSets[A](subset: Set[A], superset: Set[A])

object NestedSets {
  implicit def arbNestedSets[A: Arbitrary]: Arbitrary[NestedSets[A]] =
    Arbitrary(for {
      set1 <- arbitrary[Set[A]]
      set2 <- arbitrary[Set[A]]
    } yield NestedSets(set1, set1 ++ set2))

  implicit def shrinkNestedSets[A: Shrink]: Shrink[NestedSets[A]] = Shrink {
    case NestedSets(subset, superset) =>
      for {
        set1 <- shrink(subset)
        set2 <- shrink(superset)
        if set1.subsetOf(set2)
      } yield NestedSets(set1, set2)
  }
}

/** Holds a pair of sets such that `notSubset` is never a subset of `set`.
  *
  * The name is a bit awkward, but this is basically the negation of `NestedSets`. I couldn't really think of a better
  * way to call it.
  */
final case class NonNestedSets[A](notSubset: Set[A], set: Set[A])

object NonNestedSets {
  implicit def arbNonNestedSets[A: Arbitrary]: Arbitrary[NonNestedSets[A]] =
    Arbitrary(for {
      set1 <- arbitrary[Set[A]]
      set2 <- arbitrary[Set[A]]
      if !set1.subsetOf(set2)
    } yield NonNestedSets(set1, set2))

  implicit def shrinkNonNestedSets[A: Shrink]: Shrink[NonNestedSets[A]] = Shrink {
    case NonNestedSets(set1, set2) =>
      for {
        s1 <- shrink(set1)
        s2 <- shrink(set2)
        if !s1.subsetOf(s2)
      } yield NonNestedSets(s1, s2)
  }
}
