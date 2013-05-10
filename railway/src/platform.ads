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
with Ada.Strings.Unbounded;
with Generic_Platform;
with Route;use Route;
with Notice_Panel;
with Ada.Containers.Ordered_Maps;

-- #
-- # Represents a Platform for a generic Station, both for Trains and Travelers.
-- #
package Platform is

	use Ada.Strings.Unbounded;

	type Platform_Handler(
		ID		: Integer;
		S 		: access Unbounded_String;
		Panel	: access Notice_Panel.Notice_Panel_Entity) is limited new Generic_Platform.Platform_Interface with private;

		overriding procedure Enter(
			This 					: access Platform_Handler;
			Train_Descriptor_Index 	: in 	Positive;
			Action 					: in	Route.Action);

		overriding procedure Leave(
			This 					: access Platform_Handler;
			Train_Descriptor_Index 	: in 	Positive;
			Action 					: in	Route.Action);

		overriding procedure Add_Incoming_Traveler(
			This 					: access Platform_Handler;
			Traveler 				: in 	Positive);

		overriding procedure Add_Outgoing_Traveler(
			This 					: access Platform_Handler;
			Traveler 				: in 	Positive);


	type Platforms is array (Positive range <>) of access Platform_Handler;

	-- # Create a queue for Traveler type
	package Traveler_Queue_Package is new Queue(Element => Positive);

	package Train_Run_Map is new Ada.Containers.Ordered_Maps(
		Key_Type 		=> Positive,
      	Element_Type 	=> Natural);


private
	package Trains_Queue_Package is new Queue (Element => Positive);

	-- #
	-- # Protected Resource used to Regulate the access to a platform.
	-- #
	protected type Platform_Type(
		ID	: Integer;
		S 	: access Ada.Strings.Unbounded.Unbounded_String) is

		-- #
		-- #
		-- #
		procedure Leave;

		entry Enter (
			Train_Descriptor_Index 	: in 	Positive);

		procedure Add_Train(
			Train_ID 	: in 	Positive);

	private

		entry Retry (
			Train_Descriptor_Index 	: in 	Positive);

		Free : Boolean := True;

		Can_Retry : Boolean := False;

		Retry_Count : Natural := 0;

		Trains_Order : Trains_Queue_Package.Unlimited_Simple_Queue;

	end Platform_Type;


	type Platform_Handler(
		ID		: Integer;
		S 		: access Unbounded_String;
		Panel	: access Notice_Panel.Notice_Panel_Entity) is limited new Generic_Platform.Platform_Interface with record

		The_Platform	: Platform_Type(ID,S);

		-- # Queue for Arriving Traveler
		Arrival_Queue 	: access Traveler_Queue_Package.Unbounded_Queue.Queue := new Traveler_Queue_Package.Unbounded_Queue.Queue;

		-- # Queue for Travelers waiting for the train to leave
		Leaving_Queue 	: access Traveler_Queue_Package.Unbounded_Queue.Queue := new Traveler_Queue_Package.Unbounded_Queue.Queue;

		Train_Run		: Train_Run_Map.Map;

	end record;

	function Get_Run_For_Train(
			This 					: access Platform_Handler;
			Train_ID				: in	 Integer) return Integer;

	procedure Perform_Entrance(
			This 					: access Platform_Handler;
			Train_Descriptor_Index 	: in 	 Positive;
			Action 					: in	 Route.Action);

	procedure Perform_Exit(
			This 					: access Platform_Handler;
			Train_Descriptor_Index 	: in 	 Positive;
			Action 					: in	 Route.Action);
	procedure Add_Train(
			This					: access Platform_Handler;
			Train_ID 				: in 	 Positive);

end Platform;
