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
package org.neo4j.cypher.internal.commands.expressions

import org.neo4j.graphdb.NotFoundException
import org.neo4j.cypher.internal.symbols._
import org.neo4j.helpers.ThisShouldNotHappenError
import org.neo4j.cypher.internal.ExecutionContext
import org.neo4j.cypher.internal.pipes.QueryState
import org.neo4j.cypher.internal.spi.Slot

object Identifier {
  def isNamed(x: String) = !notNamed(x)

  def notNamed(x: String) = x.startsWith("  UNNAMED")
}

object NamedIdentifier {
  def unapply(x: Any): Option[String] = x match {
    case x: AbstractIdentifier => Some(x.entityName)
    case _                     => None
  }
}

sealed abstract class AbstractIdentifier extends Expression with Typed {

  def entityName: String

  def slot: Option[Slot]

  protected def newNotFoundException: NotFoundException =
    new NotFoundException("Unknown identifier `%s`.".format(entityName))

  override def toString: String = entityName

  def rewrite(f: (Expression) => Expression) = f(this)

  def children = Seq()

  def calculateType(symbols: SymbolTable) =
    throw new ThisShouldNotHappenError("Andres", "This class should override evaluateType, and this method should never be run")

  override def evaluateType(expectedType: CypherType, symbols: SymbolTable) = symbols.evaluateType(entityName, expectedType)

  def symbolTableDependencies = Set(entityName)

}

final case class Identifier(entityName: String) extends AbstractIdentifier {

  def apply(ctx: ExecutionContext)(implicit state: QueryState): Any =
    ctx.getOrElse(entityName, throw newNotFoundException)

  def slot: Option[Slot] = None
}

final case class SlotIdentifier(override val entityName: String, entitySlot: Slot) extends AbstractIdentifier {

  def apply(ctx: ExecutionContext)(implicit state: QueryState): Any =
    entitySlot.get(ctx).getOrElse(throw newNotFoundException)

  def slot: Option[Slot] = Some(entitySlot)
}
