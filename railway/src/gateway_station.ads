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

with Ada.Strings.Hash;

with Ada.Containers;use Ada.Containers;
with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Containers.Ordered_Maps;

with Queue;
with Route;
with Generic_Platform;


package Gateway_Station is

	package Trains_Queue_Package is new Queue (Element => Positive);

   	package Segments_Map is new Ada.Containers.Ordered_Maps(
   		Key_Type 		=> Positive,
      	Element_Type 	=> Access_Controller_Ref
    );

	package String_Positive_Maps is new Ada.Containers.Indefinite_Hashed_Maps(
		Key_Type 		=> String,
		Element_Type 	=> Positive,
		Hash			=> Ada.Strings.Hash,
		Equivalent_Keys => "="
	);

	package Unbounded_Strings renames Ada.Strings.Unbounded;

	use Unbounded_Strings;

	-- #
	-- # Definition of Regional Station Type implementing Station_Interface --
	-- #
	type Gateway_Station_Type(Platforms_Number : Positive) is limited
		new Ada.Finalization.Limited_Controlled
		and Station_Interface
	with private;

		overriding procedure Enter(
			This 				: in out	Gateway_Station_Type;
			Descriptor_Index	: in		Positive;
			Platform_Index		: in		Positive;
			Segment_ID			: in 		Positive;
			Action				: in 		Route.Action);

		overriding procedure Leave(
			This 				: in out	Gateway_Station_Type;
			Descriptor_Index	: in		Positive;
			Platform_Index		: in		Positive;
			Action				: in 		Route.Action);

		overriding procedure Wait_For_Train_To_Go(
			This 				: in out	Gateway_Station_Type;
			Outgoing_Traveler 	: in		Positive;
			Train_ID 			: in		Positive;
			Platform_Index		: in		Positive);

		overriding procedure Wait_For_Train_To_Arrive(
			This 				: in out	Gateway_Station_Type;
			Incoming_Traveler 	: in		Positive;
			Train_ID 			: in		Positive;
			Platform_Index		: in		Positive);

		overriding procedure Terminate_Platforms(
			This 				: in out 	Gateway_Station_Type);

		-- #
		-- # Class method used by segments to add a Train to the proper order queue, to allow
		-- # future ordered access.
		-- #
		overriding procedure Add_Train(
			This				: in out	Gateway_Station_Type;
			Train_ID			: in 		Positive;
			Segment_ID			: in 		Positive);

		overriding procedure Buy_Ticket(
			This				: in out	Gateway_Station_Type;
			Traveler_Index		: in		Positive;
			To					: in 		String);

		overriding function Get_Name(
			This				: in out	Gateway_Station_Type) return String;


		procedure Occupy_Platform(
			This				: in out Gateway_Station_Type;
			Platform_Index		: in 	 Positive;
			Train_Index			: in 	 Positive);


	-- #
	-- # Creates a new Station instance
	-- #
	-- # @return: A reference of the new created Station
	-- #
	function New_Gateway_Station(
		Platforms_Number 	: Positive;
		Name 				: String;
		Destinations		: access String_Positive_Maps.Map) return Station_Ref;

	-- #
	-- # Print method used for debug purposes
	-- #
	procedure Print(This : Gateway_Station_Type);
--
--  	----------------------- Procedures to convert Json to Station ------------------------
--
--  	-- #
--  	-- # Creates a Regional_Station_Type object containing the station defined in the given Json_Value
--  	-- #
--  	-- # @return A reference to the created Regional_Station_Type object
--  	-- #
	function Get_Gateway_Station(Json_Station : Json_Value) return Station_Ref;


	-- #
	-- # Overriding of the Finalize method.
	-- #
	overriding procedure Finalize(This: in out Gateway_Station_Type);

private

	type Gateway_Station_Type(Platforms_Number : Positive) is limited
		new Ada.Finalization.Limited_Controlled and
		Station_Interface
	with record
		Name 				: aliased  Unbounded_Strings.Unbounded_String;
		Platforms 			: Platform.Platforms(1..Platforms_Number);
		Panel 				: access Notice_Panel.Notice_Panel_Entity := null;
		Segments_Map_Order	: Segments_Map.Map;
		Destinations 		: access String_Positive_Maps.Map := null;
	end record;

end Gateway_Station;
