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
package org.neo4j.cypher.internal.spi

import scala.collection.mutable
import org.neo4j.cypher.CypherException
import org.neo4j.cypher.internal.ExecutionContext

sealed abstract class Slot

abstract class SlotTracker extends (String => Option[Slot]) {

  def all: Iterable[Slot]

  def +=(name: String): Slot
  def -=(name: String): Slot

  def contains(name: String): Boolean
}

class SlotAccess {
  def put(slot: Slot)(m: ExecutionContext, value: Any) = m += nameSlot(slot).name -> value
  def get(slot: Slot)(m: ExecutionContext): Any = m.apply(nameSlot(slot).name)
  def getOption(slot: Slot)(m: ExecutionContext): Option[Any] = m.get(nameSlot(slot).name)
  def remove(slot: Slot)(m: ExecutionContext): Any = m -= nameSlot(slot).name

  private def nameSlot(slot: Slot) = slot.asInstanceOf[NameSlot]

}

sealed abstract class SlotException(slotName: String, message: String) extends CypherException(message, null)

class DuplicateSlotException(slotName: String, slot: Slot)
  extends SlotException(slotName, s"Slot for '$slotName' already exists")

class SlotNotFoundException(slotName: String)
  extends SlotException(slotName, s"Slot for '$slotName' not found")

final case class NameSlot(name: String) extends Slot

class NameSlotTracker extends SlotTracker {
  private val m: collection.mutable.Map[String, NameSlot] = new mutable.LinkedHashMap[String, NameSlot]

  def all: Iterable[Slot] = m.values

  def +=(name: String): Slot =
    if (m.contains(name)) {
      throw new DuplicateSlotException(name, m(name))
    } else {
      val slot = NameSlot(name)
      m += name -> slot
      slot
    }

  def -=(name: String): Slot =
    if (m.contains(name)) {
      val slot = m(name)
      m -= name
      slot
    } else {
      throw new SlotNotFoundException(name)
    }

  def contains(name: String): Boolean = m.contains(name)

  def apply(name: String): Option[Slot] = m.get(name)
}

