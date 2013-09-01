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

import org.junit.Test
import org.scalatest.Assertions
import org.neo4j.cypher.internal.commands.expressions.{SlotIdentifier, Identifier}

class SlotTrackerTest extends Assertions {

  @Test
  def should_register_slots() {
    // given
    val tracker = createTracker

    // when
    val slot = tracker += "slot"

    // then
    assert( Some(slot) === tracker("slot"))
  }

  @Test
  def should_not_fabricate_slots() {
    // given
    val tracker = createTracker

    // when
    try {
      tracker.get("slot")
      fail("Expected SlotNotFoundException")
    } catch {
      // then
      case e : SlotNotFoundException =>
        assert( "slot" === e.slotName )
    }
  }

  @Test
  def should_not_register_slots_twice() {
    // given
    val tracker = createTracker
    val slot = tracker += "slot"

    // when
    try {
      tracker += "slot"
      fail("Expected DuplicateSlotException")
    } catch {
      // then
      case e : DuplicateSlotException =>
        assert( "slot" === e.slotName )
        assert( slot === e.slot )
    }
  }

  @Test
  def should_unRegister_slots() {
    // given
    val tracker = createTracker

    // when
    tracker += "slot"
    tracker -= "slot"

    // then
    assert( None === tracker("slot"))
  }

  @Test
  def should_not_unRegister_missing_slots() {
    // given
    val tracker = createTracker

    // when
    try {
      tracker -= "slot"
      fail("Expected SlotNotFoundException")
    }
    catch {
      // then
      case e: SlotNotFoundException =>
        assert( "slot" === e.slotName )
    }
  }

  @Test
  def should_optionally_register_slots() {
    // given
    val tracker = createTracker

    // when
    val slot1 = tracker +?= "slot"
    val slot2 = tracker +?= "slot"

    // then
    assert( slot1 === slot2 )
  }

  @Test
  def should_map_identifiers() {
    // given
    val tracker = createTracker
    val slot = tracker += "slot"

    // when
    val result = tracker.mapExpression(Identifier("slot"))

    // then
    result match {
      case SlotIdentifier("slot", s) =>
        assert( slot === s )
      case _  =>
        fail("Did not rewrite identifier to slot identifier in expression")
    }
  }

  def createTracker: SlotTracker = new NameSlotTracker
}