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
with Route;use Route;
with Time_Table;

package Routes is

	All_Routes : access Route.Routes := null;

	-- #
    -- #  Procedure used to load routes array.
	-- #
	procedure Init;

	type Routes_Indexes is array (Positive range <>) of Positive;

	-- #
	-- # Functions which tells if the given route contains a stage that can take a Traveler
	-- # from [From] to [To]. If this stage exists, return its index.
	-- #
	function Contains(
		Route_Index : in 	Positive;
		From		: in 	Positive;
		To			: in 	Positive) return Natural;

	-- #
	-- # Returns an array of routes indexes; these routes contain a stage which goes from
	-- # [From] to [To].
	-- #
	function Get_Routes_Containing(
		From		: in 	Positive;
		To			: in 	Positive) return Routes_Indexes;


	-- #
	-- # Returns an array of indexes of Routes traveled by FB trains
	-- #
--  	function Get_FB_Routes return access Route.Routes;


end Routes;
