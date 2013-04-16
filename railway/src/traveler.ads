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

with Ada.Strings.Unbounded;
with Generic_Operation_Interface;use Generic_Operation_Interface;
with JSON_Helper;
with Gnatcoll.JSON;use Gnatcoll.JSON;
with JSON_Helper;use JSON_Helper;
with Ticket;

package Traveler is

	use Ada.Strings.Unbounded;

	type Traveler_Operations_Types is (BUY_TICKET,LEAVE,ENTER,TICKET_READY);

	type Traveler_Operations is Array(Traveler_Operations_Types range <>) of Any_Operation;

	type Traveler_Operations_Ref is access all Traveler_Operations;

	type Travelers_All_Operations is array (Positive range <>) of Traveler_Operations_Ref;

	type Travelers_All_Operations_Ref is access all Travelers_All_Operations;

	-- # Traveler type declaration
	type Traveler_Type is record
		ID 			: Integer;
		Name 		: Unbounded_String;
		Surname 	: Unbounded_String;
	end record;

	-- # Traveler Manager
	type Traveler_Manager is record
		Traveler 		: Traveler_Type;
		Next_Operation 	: Traveler_Operations_Types := LEAVE;
		Destination 	: Unbounded_String;
		Ticket 			: access Ticket.Ticket_Type := null;
		Start_Station 	: Positive;
	end record;

	type Traveler_Manager_Array is Array(Positive range <>) of aliased Traveler_Manager;

	type Traveler_Manager_Array_Ref is access all Traveler_Manager_Array;

	function Get_Name(Traveler : Traveler_Manager) return String;

	procedure Print(T : Traveler_Manager);

	function Get_Traveler_Manager_Array(Json_Traveler : String) return Traveler_Manager_Array_Ref;

	function Get_Traveler_Manager(Json_Traveler : String) return Traveler_Manager;

	function Get_Json(
		Traveler : Traveler_Manager) return String;

private

	-- #
	-- # Creates a Traveler from JSON object
	-- #
	-- # @return : A Traveler_Type object
	-- #
	function Get_Traveler(Json_Traveler : JSON_Value) return Traveler_Type;

	-- #
	-- # Creates a Traveler_Manager from JSON object
	-- #
	-- # @return : A Traveler_Manager object
	-- #
	function Get_Traveler_Manager(Json_Traveler : JSON_Value) return Traveler_Manager;

end Traveler;
