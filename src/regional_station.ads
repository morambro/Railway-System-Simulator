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

package Regional_Station is

	package Unbounded_Strings renames Ada.Strings.Unbounded;

	use Unbounded_Strings;

	-- #
	-- # Array Containing Platforms references
	-- #
	type Platforms_List is array (Positive range <>) of access Platform.Platform_Type;

	type Platform_Booking is array (Positive range <>) of Boolean;

	-- #
	-- # Definition of Regional Station Type implementing Station_Interface --
	-- #
	type Regional_Station_Type(Platforms_Number : Positive) is
		new Ada.Finalization.Controlled
		and Station_Interface
	with private;

		overriding procedure Enter(
			This : Regional_Station_Type;
			Descriptor : in out Train.Train_Descriptor;
			Platform: Integer);

		overriding procedure Leave(
			This : Regional_Station_Type;
			Descriptor : in out Train.Train_Descriptor;
			Platform: Integer);

		overriding procedure Wait_For_Train(
			This 				: in		Regional_Station_Type;
			Outgoing_Traveler 	: in		Positive;
			Train_ID 			: in		Positive;
			Platform_Index		: in		Positive);

		overriding function Get_Platform(
			This : Regional_Station_Type;
			P : Natural) return access Platform.Platform_Type;

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
		Name : Unbounded_Strings.Unbounded_String;
		Platforms : Platforms_List(1..Platforms_Number);
		Panel : access Notice_Panel.Notice_Panel_Entity := null;
		-- Indicates for each platform if it is free or not
		Platform_Free : Platform_Booking(1 .. Platforms_Number) := (others => true);
	end record;

end Regional_Station;
