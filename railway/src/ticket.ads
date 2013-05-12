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
with Ada.Unchecked_Deallocation;
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
		-- # The current run progressive number, to be specified if the Train is a FB train
		Current_Run_Id				: Integer := 0;
		-- # Destination platform index
		Destination_Platform_Index	: Natural := 0;
    end record;

    type Ticket_Stages is array (Positive range <>) of Ticket_Stage;

    type Ticket_Type is record
    	Next_Stage 	: Positive := 1;
    	Stages 		: access Ticket_Stages := null;
	end record;

	type Ticket_Type_Ref is access all Ticket_Type;

	procedure Print(
		The_Ticket : Ticket_Type_Ref);

	-- #
	-- # Procedure which can be used to explicitly delete
	-- # a ticket.
	-- #
	procedure Free_Ticket is new Ada.Unchecked_Deallocation
      (Object => Ticket_Type, Name => Ticket_Type_Ref);

    -- ############################## Json -> Ticket ###############################

    function Get_Ticket (
    	Json_String	: in String) return Ticket_Type_Ref;

    function Get_Ticket (
    	Json_V 		: in JSON_Value) return Ticket_Type_Ref;

    function To_Json(
		Ticket 		: in Ticket_Type_Ref) return String;


end Ticket;