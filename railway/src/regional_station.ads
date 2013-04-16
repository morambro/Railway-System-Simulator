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

with Generic_Station;use Generic_Station;
with Train;
with Platform;
with Traveler;
with Notice_Panel;
with Gnatcoll.JSON;use Gnatcoll.JSON;
with JSON_Helper;use JSON_Helper;
with Ada.Strings.Unbounded;

with Ada.Finalization;
with Unchecked_Deallocation;

with Ada.Containers.Ordered_Maps;  use Ada.Containers;
with Ada.Containers.Vectors;
with Queue;
with Route;
with Generic_Platform;

package Regional_Station is


	-- ############################### ACCESS_CONTROLLER ########################################

	package Trains_Queue_Package is new Queue (Element => Positive);

	protected type Access_Controller is

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

	-- ##########################################################################################


   	package Segments_Map is new Ada.Containers.Ordered_Maps(
   		Key_Type 		=> Positive,
      	Element_Type 	=> Access_Controller_Ref
    );


	package Unbounded_Strings renames Ada.Strings.Unbounded;

	use Unbounded_Strings;


	-- #
	-- # Definition of Regional Station Type implementing Station_Interface --
	-- #
	type Regional_Station_Type(Platforms_Number : Positive) is
		new Ada.Finalization.Controlled
		and Station_Interface
	with private;

		overriding procedure Enter(
			This 				: in		Regional_Station_Type;
			Descriptor_Index	: in		Positive;
			Platform_Index		: in		Positive;
			Segment_ID			: in 		Positive;
			Action				: in 		Route.Action);

		overriding procedure Leave(
			This 				: in 		Regional_Station_Type;
			Descriptor_Index	: in		Positive;
			Platform_Index		: in		Positive;
			Action				: in 		Route.Action);

		overriding procedure Wait_For_Train_To_Go(
			This 				: in		Regional_Station_Type;
			Outgoing_Traveler 	: in		Positive;
			Train_ID 			: in		Positive;
			Platform_Index		: in		Positive);

		overriding procedure Wait_For_Train_To_Arrive(
			This 				: in		Regional_Station_Type;
			Incoming_Traveler 	: in		Positive;
			Train_ID 			: in		Positive;
			Platform_Index		: in		Positive);

		overriding procedure Add_Train(
			This				: in 		Regional_Station_Type;
			Train_ID			: in 		Positive;
			Segment_ID			: in 		Positive);

		overriding function Get_Name(
			This				: in 		Regional_Station_Type) return String;

		overriding procedure Buy_Ticket(
			This				: in 		Regional_Station_Type;
			Traveler_Index		: in		Positive;
			To					: in 		String);

	-- #
	-- # Creates a new Station instance
	-- #
	-- # @return: A reference of the new created Station
	-- #
	function New_Regional_Station(
		Platforms_Number : Positive;
		Name : String) return Station_Ref;

	-- #
	-- # Print method used for debug purposes
	-- #
	procedure Print(This : Regional_Station_Type);

	----------------------- Procedures to convert Json to Station ------------------------

	-- #
	-- # Creates a Regional_Station_Type object containing the station defined in the given Json_Value
	-- #
	-- # @return A reference to the created Regional_Station_Type object
	-- #
	function Get_Regional_Station(Json_Station : Json_Value) return Station_Ref;

	-- #
	-- # Creates a Station_Array object containing the stations defined in the given Json_Value
	-- #
	-- # @return A reference to the created Stations_Array
	-- #
	function Get_Regional_Station_Array(Json_Station : String) return Stations_Array_Ref;

	--------------------------------------------------------------------------------------

	-- #
	-- # Overriding of the Finalize method.
	-- #
	overriding procedure Finalize(This: in out Regional_Station_Type);

private

	type Regional_Station_Type(Platforms_Number : Positive) is
		new Ada.Finalization.Controlled and
		Station_Interface
	with record
		Name 				: aliased Unbounded_Strings.Unbounded_String;
		Platforms 			: Platform.Platforms(1..Platforms_Number);
		Panel 				: access Notice_Panel.Notice_Panel_Entity := null;
		Segments_Map_Order	: access Segments_Map.Map := new Segments_Map.Map;
	end record;

end Regional_Station;
