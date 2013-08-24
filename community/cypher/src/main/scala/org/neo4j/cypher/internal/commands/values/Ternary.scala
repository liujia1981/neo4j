/**
 * Copyright (c) 2002-2013 "Neo Technology,"
 * Network Engine for Objects in Lund AB [http://neotechnology.com]
 *
 * This file is part of Neo4j.
 *
 * Neo4j is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */
package org.neo4j.cypher.internal.commands.values

sealed abstract class Ternary(val isKnown: Boolean, val isTrue: Boolean, val isFalse: Boolean)
  extends (Any => Boolean) {
  val inversed: Ternary

  def isTrueOrElse(orElse: Boolean): Boolean = isTrue || orElse
  def isFalseOrElse(orElse: Boolean): Boolean = isFalse || orElse
  def toKnownOption: Option[Ternary] = if (isKnown) Some(this) else None
  def and(other: Ternary): Ternary
  def or(other: Ternary): Ternary
  def xor(other: Ternary): Ternary
  def unapply(value: Any): Option[Ternary] = if (apply(value)) Some(this) else None
}

object Ternary {
  def fromBoolean(b: Boolean) = if (b) IsTrue else IsFalse
  def fromValue(v: Any): Ternary = v match {
    case b: Boolean => fromBoolean(b)
    case v: Ternary => v
    case _          => IsUnknown
  }

  val values = Set(IsTrue, IsFalse, IsUnknown)
}

object IsKnown {
  def unapply(v: Any) = Ternary.fromValue(v).toKnownOption
}

case object IsTrue extends Ternary(isKnown = true, isTrue = true, isFalse = false) {
  override val inversed: Ternary = IsFalse

  override def and(other: Ternary): Ternary = other
  override def or(other: Ternary): Ternary = IsTrue
  override def xor(other: Ternary): Ternary = if (other.isKnown) Ternary.fromBoolean(other.isFalse) else IsUnknown
  override def apply(other: Any): Boolean = true == other || IsTrue == other
  override def toString() = "true"
}

case object IsFalse extends Ternary(isKnown = true, isTrue = false, isFalse = true) {
  override val inversed: Ternary = IsTrue

  override def and(other: Ternary): Ternary = IsFalse
  override def or(other: Ternary): Ternary = other
  override def xor(other: Ternary): Ternary = if (other.isKnown) Ternary.fromBoolean(other.isTrue) else IsUnknown
  override def apply(other: Any): Boolean = false == other || IsFalse == other
  override def toString() = "false"
}

/**
 * This singleton value represents unbound entities inside expressions or predicates.
 *
 * It currently only may occur due to patterns containing optional relationships which may introduce
 * unbound identifiers.  It mainly serves to differentiate this situation from plain null values.
 */
case object IsUnknown extends Ternary(isKnown = false, isTrue = false, isFalse = false) {
  override val inversed: Ternary = IsUnknown

  override def isTrueOrElse(orElse: Boolean): Boolean = orElse
  override def isFalseOrElse(orElse: Boolean): Boolean = orElse
  override def and(other: Ternary): Ternary = if (other.isFalse) IsFalse else IsUnknown
  override def or(other: Ternary): Ternary = if (other.isTrue) IsTrue else IsUnknown
  override def xor(other: Ternary): Ternary = IsUnknown
  override def apply(other: Any): Boolean = IsUnknown == other
  override def toString() = "unknown"
}