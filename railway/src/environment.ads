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

with Segment;
with Generic_Station;
with Regional_Station;
with Traveler;
with Move_Operation;
with Ticket;
with Gateway_Station;
with Ada.Strings.Unbounded;use Ada.Strings.Unbounded;

-- #
-- # This package contains environment's objects, in order to be retrieved easily from each
-- # other component
-- #
package Environment Is

    function Get_Regional_Stations return Generic_Station.Stations_array_Ref;

    function Get_Travelers return Traveler.Traveler_Manager_Array_Ref;

    function Get_Operations return Traveler.Travelers_All_Operations_Ref;

	function Get_Node_Name return String;

	function Get_Name_Server return String;

    procedure Init(N_N : in String;N_S : in String);

private

    Node_Name : Unbounded_String;

    Name_Server : Unbounded_String;

end Environment;
