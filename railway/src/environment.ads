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
with Time_Table;

WITH YAMI.Parameters;

-- #
-- # This package contains environment's objects, in order to be retrieved easily from each
-- # other component
-- #
package Environment Is

	-- # All the Stations
	Stations 		: Generic_Station.Stations_array_Ref := null;

	-- # All the Travelers
    Travelers 		: Traveler.Traveler_Manager_Array_Ref := null;

	-- # Operations for each Traveler
	Operations 		: Traveler.Travelers_All_Operations_Ref := null;

	-- # Time Table
	T			 	: Time_Table.Time_Table_Array := Time_Table.Get_Time_Table_Array("res/time_table.json");

	function Get_Node_Name return String;

	function Get_Name_Server return String;

	function Get_Central_Ticket_Office return String;

	-- #
	-- # Environment package initialization.
	-- #
    procedure Init(
    	N_N 		: in 	String;
    	N_S 		: in 	String;
    	C_T			: in 	String);

	-- #
	-- # Procedure used to update traveler information.
	-- #
	procedure Update_Traveler(
		Traveler_Index	: in 		Positive;
		Trav_To_Copy 	: in		Traveler.Traveler_Manager;
		Ticket_To_Copy 	: access 	Ticket.Ticket_Type);

	-- #
	-- # Function used to get the index of the Station, given its unique name.
	-- #
	function Get_Index_For_Name(
		Name 			: in 	String) return Natural;

private

    Node_Name : Unbounded_String;

    Name_Server : Unbounded_String;

    Central_Ticket_Office : Unbounded_String;

end Environment;
