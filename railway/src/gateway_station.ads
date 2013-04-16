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
with Gateway_Platform;
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

	-- ############################### ACCESS_CONTROLLER ########################################

	-- #
	-- # Protected resource type; it defines object used to maintain an access order to
	-- # the platforms, for all the tasks coming from the same Segment.
	-- #
	protected type Access_Controller is

		-- #
		-- # Entry called by the train task to regulate the entrance order for a Segment
		-- #
		entry Enter(
			Train_Index	: in 	Positive);

		-- #
		-- # Simply Adds the Given Train ID to the internal Queue
		-- #
		procedure Add_Train(
			Train_ID 	: in 	Positive);

		-- #
		-- # This procedure Frees the Access Controller
		-- #
		procedure Free;

	private

		-- #
		-- # Private Entry used to enqueue trains which can not enter the
		-- # platform because of the specified order.
		-- #
		entry Wait(
			Train_Index	: in 	Positive);

		-- # The unlimited queue used to store train indexes
		Trains_Order 	: Trains_Queue_Package.Unlimited_Simple_Queue;

		-- # Boolean variable used as a guard for Wait entry
		Can_Retry 		: Boolean := False;

		-- # Natural field used to store the amount of Trains waiting by Wait entry
		-- # before opening it.
		Trains_Waiting 	: Natural := 0;

 	end Access_Controller;

	-- #
	-- # Reference type for Access Controller.
	-- #
 	type Access_Controller_Ref is access all Access_Controller;


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


	package String_String_Maps is new Ada.Containers.Indefinite_Hashed_Maps(
		Key_Type 		=> String,
		Element_Type 	=> String,
		Hash			=> Ada.Strings.Hash,
		Equivalent_Keys => "="
	);

	-- # String-String Map used as a cache to maintain last addresses
	Last_Addresses : String_String_Maps.Map;


	package Unbounded_Strings renames Ada.Strings.Unbounded;

	use Unbounded_Strings;

	-- #
	-- # Definition of Regional Station Type implementing Station_Interface --
	-- #
	type Gateway_Station_Type(Platforms_Number : Positive) is
		new Ada.Finalization.Controlled
		and Station_Interface
	with private;

		overriding procedure Enter(
			This 				: in		Gateway_Station_Type;
			Descriptor_Index	: in		Positive;
			Platform_Index		: in		Positive;
			Segment_ID			: in 		Positive;
			Action				: in 		Route.Action);

		overriding procedure Leave(
			This 				: in 		Gateway_Station_Type;
			Descriptor_Index	: in		Positive;
			Platform_Index		: in		Positive;
			Action				: in 		Route.Action);

		overriding procedure Wait_For_Train_To_Go(
			This 				: in		Gateway_Station_Type;
			Outgoing_Traveler 	: in		Positive;
			Train_ID 			: in		Positive;
			Platform_Index		: in		Positive);

		overriding procedure Wait_For_Train_To_Arrive(
			This 				: in		Gateway_Station_Type;
			Incoming_Traveler 	: in		Positive;
			Train_ID 			: in		Positive;
			Platform_Index		: in		Positive);

		-- #
		-- # Class method used by segments to add a Train to the proper order queue, to allow
		-- # future ordered access.
		-- #
		overriding procedure Add_Train(
			This				: in 		Gateway_Station_Type;
			Train_ID			: in 		Positive;
			Segment_ID			: in 		Positive);

		overriding procedure Buy_Ticket(
			This				: in 		Gateway_Station_Type;
			Traveler_Index		: in		Positive;
			To					: in 		String);

		overriding function Get_Name(
			This				: in 		Gateway_Station_Type) return String;

		-- #
		-- # Class method used to Transfer a Train via remote message to a given destination.
		-- #
		procedure Send_Train(
			This					: in 	 Gateway_Station_Type;
			Train_Descriptor_Index 	: in	 Positive;
			Station	 				: in	 Positive;
			Platform				: in 	 Positive;
			Node_Name				: in	 String );

		-- #
		-- # Class method used to Send an Acknowledge message to notify the sender Gateway Station
		-- # that the sent Train left the destination Gateway Station.
		-- #
		procedure Send_Ack(
			This					: in 	 Gateway_Station_Type;
			Train_Descriptor_Index 	: in	 Positive;
			Station	 				: in	 Positive;
			Platform				: in 	 Positive;
			Node_Name				: in	 String );

		procedure Occupy_Platform(
			This					: in 	 Gateway_Station_Type;
			Platform_Index			: in 	 Positive;
			Train_Index				: in 	 Positive);


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


	procedure Send_Traveler_To_Leave(
		Traveler_Index	: in	 Positive;
		Train_ID		: in 	 Positive;
		Station	 		: in	 Positive;
		Platform		: in 	 Positive;
		Node_Name		: in	 String );



	-- #
	-- # Overriding of the Finalize method.
	-- #
	overriding procedure Finalize(This: in out Gateway_Station_Type);

private

	type Gateway_Station_Type(Platforms_Number : Positive) is
		new Ada.Finalization.Controlled and
		Station_Interface
	with record
		Name 				: aliased  Unbounded_Strings.Unbounded_String;
		Platforms 			: Gateway_Platform.Platforms(1..Platforms_Number);
		Panel 				: access Notice_Panel.Notice_Panel_Entity := null;

		Segments_Map_Order	: access Segments_Map.Map := new Segments_Map.Map;

		Destinations 		: access String_Positive_Maps.Map := null;
	end record;

end Gateway_Station;
