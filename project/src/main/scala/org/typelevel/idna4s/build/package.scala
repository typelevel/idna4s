/*
 * Copyright 2023 Typelevel
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

package org.typelevel.idna4s

import cats._
import cats.syntax.all._
import cats.data._
import scala.collection.immutable.SortedMap

package object build {

  /**
   * Utility method to flatten a map of A -> F[B], where F is some non-empty type such as
   * `NonEmptyChain` into a mapping of A -> B. This will fail if there is more than one `B` for
   * a given `A`.
   */
  private[build] def flattenValues[F[_]: Reducible, A: Order, B: Order](
      fa: SortedMap[A, F[B]]): Either[NonEmptyMap[A, NonEmptySet[B]], SortedMap[A, B]] = {
    implicit val orderingForA: Ordering[A] = Order[A].toOrdering
    fa.foldLeft((SortedMap.empty[A, NonEmptySet[B]], SortedMap.empty[A, B])) {
      case ((errors, acc), (key, values)) =>
        val valuesSet: NonEmptySet[B] =
          values.reduceMap((b: B) => NonEmptySet.one(b))
        if (valuesSet.size === 1) {
          (errors, acc + (key -> valuesSet.head))
        } else {
          (errors + (key -> valuesSet), acc)
        }
    } match {
      case (errors, acc) =>
        NonEmptyMap
          .fromMap(errors)
          .fold(
            // Empty, no duplicate values
            Right(acc): Either[NonEmptyMap[A, NonEmptySet[B]], SortedMap[A, B]]
          )(errors => Left(errors))
    }
  }

  private[build] def flattenValuesOrError[F[_]: Reducible, A: Order: Show, B: Order: Show](
      fa: SortedMap[A, F[B]]
  ): Either[String, SortedMap[A, B]] =
    flattenValues[F, A, B](fa).leftMap(errors =>
      s"Error, found the follow key mappings with more than one distinct value: ${errors.show}")

  /**
   * For a given `SortedMap`, invert the mapping.
   */
  private def invertMapping[A: Order, B: Ordering: Order](
      value: SortedMap[A, B]): SortedMap[B, NonEmptySet[A]] =
    value.toList.foldMap {
      case (k, v) =>
        SortedMap(v -> NonEmptySet.one(k))
    }

  /**
   * For a map of code point range values to some type `A`, collapse ranges of code points which
   * map to the same `A` into a single code point range. This becomes useful when we are
   * interested in a certain sub-property of a code point, for example the "General_Category".
   *
   * By grouping these values when we can, we make the size of the generated code smaller.
   */
  private[build] def group[A: Eq: Order](
      value: SortedMap[CodePointRange, A]): SortedMap[CodePointRange, A] = {
    implicit def ordering: Ordering[A] = Order[A].toOrdering
    invertMapping(value).foldLeft(SortedMap.empty[CodePointRange, A]) {
      // Remember we just inverted this.
      case (acc, (value, keys)) =>
        keys
          .foldLeft((acc, Option.empty[CodePointRange])) {
            case ((acc, previous), key) =>
              previous.fold(
                (acc + (key -> value), Some(key))
              )(previous =>
                if (previous.upper.value < Character.MAX_CODE_POINT && (previous
                    .upper
                    .value + 1) === key.lower.value) {
                  val newRange: CodePointRange =
                    CodePointRange.unsafeFrom(previous.lower, key.upper)
                  ((acc - previous) + (newRange -> value), Some(newRange))
                } else {
                  (acc + (key -> value), Some(key))
                })
          }
          ._1
    }
  }
}
