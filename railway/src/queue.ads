----------------------------------------------------------------------------------
--  Copyright 2013                                								--
--  Moreno Ambrosin                         									--
--  Railway_Simulation 1.0                                       				--
--  Concurrent and Distributed Systems class project  							--
--  Master Degree in Computer Science                 							--
--  Academic year 2012/2013                              						--
--  Dept. of Pure and Applied Mathematics             							--
--  University of Padua, Italy                        							--
--                                                    							--
--  This file is part of Railway_Simulation project.							--
--																				--
--  Railway_Simulation is free software: you can redistribute it and/or modify	--
--  it under the terms of the GNU General Public License as published by		--
--  the Free Software Foundation, either version 3 of the License, or			--
--  (at your option) any later version.											--
--																				--
--  Railway_Simulation is distributed in the hope that it will be useful,		--
--  but WITHOUT ANY WARRANTY; without even the implied warranty of				--
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the				--
--  GNU General Public License for more details.								--
--																				--
--  You should have received a copy of the GNU General Public License			--
--  along with Railway_Simulation.  If not, see <http://www.gnu.org/licenses/>. --
----------------------------------------------------------------------------------

with Ada.Containers.Unbounded_Synchronized_Queues;
with Ada.Containers.Synchronized_Queue_Interfaces;
with Ada.Finalization;
with Ada.Containers.Vectors;
--
-- Declaration of a generic queue implementation of elements of type Element
--
generic type Element is private;

package Queue is

	package Unbounded_Queue_Interface is new Ada.Containers.Synchronized_Queue_Interfaces(Element_Type => Element);

	package Unbounded_Queue is new Ada.Containers.Unbounded_Synchronized_Queues
		(Queue_Interfaces => Unbounded_Queue_Interface);

	-- ################ TERMINABLE_UNBOUNDED_QUEUE:

	protected type Terminable_Queue is
		procedure Enqueue(
			To_Add		: in 	 Element);

		entry Dequeue(
			To_Get		: 	 out Element;
			Terminated 	: 	 out Boolean);

		procedure Stop;

	private
		Termination : Boolean := False;
		Q 			: Unbounded_Queue.Queue;
    end Terminable_Queue;

	Simple_Queue_Element : Exception;

	-- ################ LIMITED_SIMPLE_QUEUE:

	type Limited_Simple_Queue_Array is array (Positive range <>) of Element;

	type Limited_Simple_Queue(Size : Positive) is tagged limited record
		Queue 			: Limited_Simple_Queue_Array(1..Size);
		Elements_Number : Natural := 0;
	end record;

		procedure Enqueue(
			This 		: in 	out Limited_Simple_Queue;
			The_Element : in		Element);

		procedure Dequeue(
			This 		: in 	out Limited_Simple_Queue;
			The_Element : 		out	Element);

		function Get(
			This 		: in		Limited_Simple_Queue;
			I 			: in 		Positive) return Element;

	-- ################ UNLIMITED_SIMPLE_QUEUE:

	package Element_Vectors is new Ada.Containers.Vectors (Positive, Element);


	type Unlimited_Simple_Queue is tagged limited record
		Queue 			: Element_Vectors.Vector;
		Elements_Number : Natural := 0;
	end record;

		procedure Enqueue(
			This 		: in out Unlimited_Simple_Queue;
			The_Element : in	 Element);

		procedure Dequeue(
			This 		: in out Unlimited_Simple_Queue;
			The_Element : 	 out	Element);

		function Get(
			This 		: in out Unlimited_Simple_Queue;
			Index 		: in	 Natural) return Element;


end Queue;
