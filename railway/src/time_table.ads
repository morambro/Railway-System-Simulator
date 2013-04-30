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
with Ada.Calendar; use Ada.Calendar;
with Gnatcoll.JSON; use Gnatcoll.JSON;

-- #
-- # This package contains the time table definition for each route, that will be used by
-- # the Trains.
-- #
package Time_Table is

	-- # Array of Time values
	type Time_Array is array (Positive range <>) of Time;

	-- # An array of Time_Array objects.
	type Time_Matrix is array (Positive range <>) of access Time_Array;

	-- # This represents a time table for a specific Route.
	type Time_Table_Type(Entry_Size : Positive) is record
		-- # Index of the route for which the Time table is specified.
		Route_Index 			: Positive;
		-- # Cursors.
		Current_Run 			: Positive := 1;
		Current_Run_Cursor 		: Positive := 1;
		-- # The time table.
		Table 					: Time_Matrix(1..Entry_Size);
    end record;

	-- # Type for the entire Time Table.
	type Time_Table_Array is array (Positive range <>) of access Time_Table_Type;

	type Time_Table_Array_Ref is access all Time_Table_Array;

	-- #
	-- # Updates the Time Table. If the Cursors are on the last position of the matrix,
	-- # it is re-calculated.
	-- #
	procedure Update_Time_Table(
		This 	: access Time_Table_Type);

	-- #
	-- # Creates a Time_Table_Type object from JSON.
	-- #
	function Get_Time_Table(Json_v : JSON_Value) return access Time_Table_Type;

	-- #
	-- # Creates a Time_Table_Type object from String.
	-- #
	function Get_Time_Table(Json_v : String) return access Time_Table_Type;

	function Get_Time_Table_Array(Json_File : String) return Time_Table_Array_Ref;

	function Get_Time_Representation(T : Time) return String;


	-- #
	-- # Simple Debug procedure
	-- #
	procedure Print(T : Time_Table_Array_Ref);

end Time_Table;