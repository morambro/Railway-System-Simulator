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

with Ada.Containers;use Ada.Containers;
with Ada.Text_IO;use Ada.Text_IO;

package body Queue is

	protected body Terminable_Queue is

		procedure Enqueue(To_Add	: in Element) is
		begin
			Q.Enqueue(To_Add);
		end Enqueue;

		entry Dequeue(
			To_Get		: 	 out Element;
			Terminated 	: 	 out Boolean) when Termination or Q.Current_Use > 0 is
		begin
			if not Termination then
				Q.Dequeue(To_Get);
			end if;
			Terminated := Termination;
		end;

		procedure Stop is
		begin
			Termination := True;
		end Stop;

    end Terminable_Queue;


   -- ################## SIMPLE_QUEUE ##########################
	procedure Enqueue(
		This 		: access Limited_Simple_Queue;
		The_Element : in	Element) is
	begin
		if This.Elements_Number = This.Size then
			raise Simple_Queue_Element with "Queue is Full!";
		end if;
		This.Queue(This.Elements_Number + 1) := The_Element;
		This.Elements_Number := This.Elements_Number + 1;
    end Enqueue;

	procedure Dequeue(
		This 		: access Limited_Simple_Queue;
		The_Element	:		out	Element) is
	begin
		The_Element := This.Queue(1);
		for I in 2 .. This.Elements_Number loop
			This.Queue(I-1) := This.Queue(I);
		end loop;
		This.Elements_Number := This.Elements_Number - 1;
    end Dequeue;

    function Get(
    	This 		: access Limited_Simple_Queue;
    	I 			: in 	Positive) return Element is
    begin
    	return This.Queue(I);
    end Get;

end Queue;
