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
with Gnatcoll.JSON; use Gnatcoll.JSON;
with Ada.Strings.Unbounded;use Ada.Strings.Unbounded;

-- #
-- # This package contains a representation of a Route for a Train as an unbounded array
-- # of Stage objects.
-- #
package Route is

	-- # Action type
	type Action is (ENTER,PASS,FREE);

	-- # Route stage
	type Stage is record
	    -- Indexes of next Segment and Station.
		Start_Station	: Positive;
		Next_Station    : Positive;
		-- # Start and Destination platforms.
		Start_Platform	: Positive;
		Platform_Index 	: Positive;
		-- # Name of the Next_Station node.
        Node_Name		: Unbounded_String;
		Next_Segment    : Positive;

		-- # Action to take at start and destination stations.
		Leave_Action	: Action;
		Enter_Action	: Action;
	end record;

	-- # Array of Stages
	type Route_Type is array (Positive range <>) of Stage;

	-- # Type representing an array of Routes.
	type Routes is array (Positive range <>) of access Route_Type;

	-- #
	-- # Function which receives a file name, and creates all the routes
	-- #
	-- # @return The corresponding Routes object
	-- #
	function Get_Routes (Json_File : String) return access Routes;

	-- #
	-- # Debug print procedure
	-- #
	procedure Print(R : Route_Type);

private

	-- #
	-- # Function which receives a JSON Value and creates an object of Route_Type.
	-- #
	-- # @return The corresponding Route_Type object
	-- #
	function Get_Route(Json_v : JSON_Value) return access Route_Type;

end Route;
