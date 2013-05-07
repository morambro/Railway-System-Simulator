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
--  Railway_Simulation is distributed in the hope that it will be useful,			--
--  but WITHOUT ANY WARRANTY; without even the implied warranty of				--
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the				--
--  GNU General Public License for more details.								--
--																				--
--  You should have received a copy of the GNU General Public License			--
--  along with Railway_Simulation.  If not, see <http://www.gnu.org/licenses/>.		--
----------------------------------------------------------------------------------
with Train;
with Traveler;

with Unchecked_Deallocation;

with Ada.Finalization;
with Generic_Platform;
with Queue;
with Route;

package Generic_Station is


	---------------------------------- STATION INTERFACE --------------------------------------
	type Station_Interface is interface;

		-- Station_Interface Methods
		procedure Enter(
			This 				: in		Station_Interface;
			Descriptor_Index	: in		Positive;
			Platform_Index		: in		Positive;
			Segment_ID			: in 		Positive;
			Action				: in 		Route.Action) is abstract;

		procedure Leave(
			This 				: in 		Station_Interface;
			Descriptor_Index	: in		Positive;
			Platform_Index		: in		Positive;
			Action				: in 		Route.Action) is abstract;

		procedure Wait_For_Train_To_Go(
			This 				: in 		Station_Interface;
			Outgoing_Traveler 	: in		Positive;
			Train_ID 			: in		Positive;
			Platform_Index		: in		Positive) is abstract;

		procedure Wait_For_Train_To_Arrive(
			This 				: in 		Station_Interface;
			Incoming_Traveler 	: in		Positive;
			Train_ID 			: in		Positive;
			Platform_Index		: in		Positive) is abstract;

		procedure Print(This : in Station_Interface) is abstract;

		procedure Add_Train(
			This				: in 		Station_Interface;
			Train_ID			: in 		Positive;
			Segment_ID			: in 		Positive) is abstract;

		procedure Buy_Ticket(
			This				: in 		Station_Interface;
			Traveler_Index		: in		Positive;
			To					: in 		String ) is abstract;

		function Get_Name(
			This				: in 		Station_Interface) return String is abstract;


	-- End Of the Interface

   	-- generic Station reference type to be used inside records: Type'Class doesn't
   	-- have a fixed size so it can be not allocated inside a record.
   	type Station_Ref is access all Station_Interface'Class;

   	type Stations_Array is array (Positive range <>) of Station_Ref;

	type Stations_Array_Ref is access Stations_Array;

	-- Code to manage memory deallocation of a Station. All objects of type < Station_Interface
	-- will be deallocated by Free method.
 	procedure Free is new Unchecked_Deallocation (
      		Station_Interface'Class,
			Station_Ref
	);

	pragma Controlled (Station_Ref);


		-- ############################### ACCESS_CONTROLLER ########################################

	package Trains_Queue_Package is new Queue (Element => Positive);

	-- #
	-- # Protected resource, which defines an object used to maintain an access order to
	-- # the platforms, for all the Train-tasks coming from the same Segment.
	-- #
	protected type Access_Controller(ID : Integer) is

		-- #
		-- # Entry called by the train task to regulate the entrance order for a Segment
		-- #
		entry Enter(
			Train_Index	: in 	 Positive);

		-- #
		-- # Simply Adds the Given Train ID to the internal Queue
		-- #
		procedure Add_Train(
			Train_ID : in 	 Positive);

		-- #
		-- # Dequeues the first element
		-- #
		procedure Free;

	private

		-- #
		-- # Private Entry used to make unordered accesses avoided.
		-- #
		entry Wait(
			Train_Index	: in 	Positive);

		-- # A simple unlimited queue used to keep track of Trains order.
		Trains_Order 	: Trains_Queue_Package.Unlimited_Simple_Queue;

		-- # Boolean Guard, used to block Tasks in Wait entry
		Can_Retry 		: Boolean := False;

		-- # A field used to store the number of tasks waiting on Wait entry
		Trains_Waiting	: Natural := 0;

 	end Access_Controller;

 	type Access_Controller_Ref is access all Access_Controller;


end Generic_Station;
