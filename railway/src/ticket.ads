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
with Gnatcoll.JSON;use Gnatcoll.JSON;
with Ada.Strings.Unbounded;use Ada.Strings.Unbounded;
-- #
-- # This package contains the definition of a ticket, that is an array of stages the
-- # traveler will follow to reach the destination.
-- #
package Ticket is

	type Ticket_Stage is record
		-- # The index of the start station
		Start_Station				: Natural := 0;
		-- # Index of the next station to reach
		Next_Station 				: Natural := 0;
		-- # The ID of the train to wait for in order to reach the destination
		Train_ID 	 				: Natural := 0;
		-- # The Platform index at which the traveler will wait
		Start_Platform_Index		: Natural := 0;
		-- # The region to which the next stage belongs to
		Region 						: Unbounded_String;
		-- # Destination platform index
		Destination_Platform_Index	: Natural := 0;
    end record;

    type Ticket_Stages is array (Positive range <>) of Ticket_Stage;

    type Ticket_Type(Stages_Number : Positive) is record
    	Next_Stage 	: Positive := 1;
    	Stages 		: Ticket_Stages(1 .. Stages_Number);
	end record;

	type Tickets_Array is array (Positive range <>) of access Ticket_Type;

    -- ############################## Json -> Ticket ###############################

    function Get_Ticket (Json_String	: in String) return access Ticket_Type;

    function Get_Ticket (Json_V 		: in JSON_Value) return access Ticket_Type;

    function Get_All_Tickets (Json_File : in String) return access Tickets_Array;


end Ticket;