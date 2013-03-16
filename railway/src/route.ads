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
--
-- This package contains a representation of a Route for a Train as an unbounded array
-- of Stage objects.
--
with Ada.Real_Time;use Ada.Real_Time;
with Gnatcoll.JSON; use Gnatcoll.JSON;
package Route is

	type Stage is private;

	type Route_Type is array (Positive range <>) of Stage;

	type Routes is array (Positive range <>) of access Route_Type;

	function Get_Next_Track (S : Stage) return Positive;
	function Get_Next_Station (S : Stage) return Positive;
	function Get_Time_To_Leave (S : Stage) return Time;
    function Get_Next_Platform (S : Stage) return Positive;

	function Get_Route(Json_v : JSON_Value) return access Route_Type;

	function Get_Routes (Json_File : String) return Routes;

	procedure Print(R : Route_Type);

private

	type Stage is record
	    -- Indexes of next Track and Station
		Next_Track      : Positive;
	    Next_Station    : Positive;
        Platform_Index 	: Positive;
		Leave_At        : Time;
	end record;


end Route;
