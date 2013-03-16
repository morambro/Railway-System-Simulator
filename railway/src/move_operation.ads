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

with Generic_Operation_Interface;use Generic_Operation_Interface;
with Traveler; use Traveler;

package Move_Operation is

	-- ############################ LEAVE OPERATION ##########################
	-- #
	-- # Operation used to make the Traveler wait for a train at a platform
	-- #
	type Leave_Operation_Type is new Operation_Interface with record
		Traveler_Manager_Index : Positive;
	end record;

	overriding procedure Do_Operation(This : in Leave_Operation_Type);

	-- ############################ ENTER OPERATION ##########################
	-- #
	-- # Operation used to make the Traveler wait at the next station to arrive
	-- #
	type Enter_Operation_Type is new Operation_Interface with record
		Traveler_Manager_Index : Positive;
	end record;

	overriding procedure Do_Operation(This : in Enter_Operation_Type);

end Move_Operation;
