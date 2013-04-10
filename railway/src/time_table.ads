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

package Time_Table is

	type Time_Array is array (Positive range <>) of Time;

	type Time_Matrix is array (Positive range <>) of access Time_Array;

	type Time_Table_Type(Entry_Size : Positive) is record
		Route_Index 			: Positive;
		Current_Array_Index 	: Positive := 1;
		Current_Array_Position 	: Positive := 1;
		Table 					: Time_Matrix(1..Entry_Size);
    end record;

	type Time_Table_Array is array (Positive range <>) of access Time_Table_Type;

	procedure Update_Time_Table(
		This 	: access Time_Table_Type);

	function Get_Time_Table(Json_v : JSON_Value) return access Time_Table_Type;

	function Get_Time_Table_Array(Json_File : String) return Time_Table_Array;

	function Get_Time_Representation(T : Time) return String;

end Time_Table;