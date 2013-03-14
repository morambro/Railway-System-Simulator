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


with Gnatcoll.JSON;use Gnatcoll.JSON;
--  with Ada.Containers.Ordered_Maps;
with Ada.Strings.Unbounded;

package JSON_Helper is

	package SU renames Ada.Strings.Unbounded;

	function Load_File(File_Name : String) return String;

	-- #
	-- # Creates a JSON_Value from a Json input. Json data can be retrieved either from file or
	-- # from a given string, it depends on what the caller specify.
	-- #
	function Get_Json_Value(Json_String : String := ""; Json_File_Name : String := "") return JSON_Value;

	procedure Handler (Name  : in UTF8_String;Value : in JSON_Value);

	procedure Print_Json(Text : String);

	procedure Print_Json_value(J : Json_Value);

--  	function "<" (a,b : SU.Unbounded_String) return Boolean;
--  	function "=" (a,b : SU.Unbounded_String) return Boolean;
--
--  	package Field_Map is new Ada.Containers.Ordered_Maps
--  		(Key_Type => SU.Unbounded_String,
--  		Element_Type => SU.Unbounded_String);


end Json_Helper;
