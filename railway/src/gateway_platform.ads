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
with Train;use Train;
with Queue;
with Traveler;use Traveler;
with Ada.Strings.Unbounded;use Ada.Strings.Unbounded;
with Generic_Platform;

-- #
-- # Represents a Platform for a generic Station, both for Trains and Travelers.
-- #
package Gateway_Platform is

	Stop_Train_Execution : Exception;

	-- # Create a queue for Traveler type
	package Traveler_Queue_Package is new Queue(Element => Positive);


	protected type Gateway_Platform_Type(
		ID	: Integer;
		S 	: access Ada.Strings.Unbounded.Unbounded_String) is new Generic_Platform.Platform_Interface with

		entry Enter(
			Train_Descriptor_Index	: in 		Positive);

		procedure Leave(
			Train_Descriptor_Index 	: in 		Positive);

		procedure Add_Incoming_Traveler(
			Traveler 	: in 		Positive);

		procedure Add_Outgoing_Traveler(
			Traveler 	: in 		Positive);

		procedure Free_Platform(
			Train_D		: in 		Positive);

		procedure Lock_Platform(
			Train_D		: in 		Positive);

	private
		Free : Boolean := True;
		-- # Queue for Arriving Traveler
		Arrival_Queue : Traveler_Queue_Package.Unbounded_Queue.Queue;

		-- # Queue for Travelers waiting for the train to leave
		Leaving_Queue : Traveler_Queue_Package.Unbounded_Queue.Queue;

		N : Integer := 3;

	end Gateway_Platform_Type;

end Gateway_Platform;
