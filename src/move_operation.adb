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

with Ada.Text_IO;use Ada.Text_IO;
with Ada.Strings.Unbounded;
with Environment;
with Logger;
with Ada.Exceptions;

package body Move_Operation is

	use Ada.Strings.Unbounded;

	NAME_LEAVE : constant String := "Move_Operation.Leave_Operation_Type";

	procedure Do_Operation(This : in Leave_Operation_Type) is
		Next_Stage 				: Positive 	:= Environment.Get_Travelers(This.Traveler_Manager_Index).Ticket.Next_Stage;
		Start_Station 			: Natural 	:= Environment.Get_Travelers(This.Traveler_Manager_Index).Ticket.Stages(Next_Stage).Start_Station;
		Train_ID	 			: Natural 	:= Environment.Get_Travelers(This.Traveler_Manager_Index).Ticket.Stages(Next_Stage).Train_ID;
		Start_Platform_Index 	: Natural 	:= Environment.Get_Travelers(This.Traveler_Manager_Index).Ticket.Stages(Next_Stage).Start_Platform_Index;
	begin
		Logger.Log(
			Sender 	=> NAME_LEAVE,
			Message => "LEAVE : Traveler " & To_String(Environment.Get_Travelers(This.Traveler_Manager_Index).Traveler.Name) &
					   " will wait at platform" & Integer'Image(Start_Platform_Index) & ", station " & Integer'Image(Start_Station),
			L 		=> Logger.NOTICE);

		Environment.Get_Regional_Stations(Start_Station).Wait_For_Train_To_Go(
			Outgoing_Traveler 	=> This.Traveler_Manager_Index,
			Train_ID 			=> Train_ID,
			Platform_Index		=> Start_Platform_Index);

	exception
		when Error : others =>
			Logger.Log(
				NAME_LEAVE,
				"Exception: " & Ada.Exceptions.Exception_Name(Error) & " , " & Ada.Exceptions.Exception_Message(Error),
				Logger.ERROR
			);
	end Do_Operation;


	-- #
	-- # Operation which lets the Traveler (Manager)
	-- #
	procedure Do_Operation(This : in Enter_Operation_Type) is
		Next_Stage 					: Positive 	:= Environment.Get_Travelers(This.Traveler_Manager_Index).Ticket.Next_Stage;
		Next_Station 				: Natural 	:= Environment.Get_Travelers(This.Traveler_Manager_Index).Ticket.Stages(Next_Stage).Next_Station;
		Train_ID	 				: Natural 	:= Environment.Get_Travelers(This.Traveler_Manager_Index).Ticket.Stages(Next_Stage).Train_ID;
		Destination_Platform_Index 	: Natural 	:= Environment.Get_Travelers(This.Traveler_Manager_Index).Ticket.Stages(Next_Stage).Destination_Platform_Index;
	begin
		Logger.Log(
			Sender 	=> NAME_LEAVE,
			Message => "ARRIVE: Traveler " & To_String(Environment.Get_Travelers(This.Traveler_Manager_Index).Traveler.Name) &
					   " will wait at platform" & Integer'Image(Destination_Platform_Index) & ", station " & Integer'Image(Next_Station),
			L 		=> Logger.NOTICE);

		Put_Line ("NEXT = " & Integer'Image(Next_Station));

		Environment.Get_Regional_Stations(Next_Station).Print;

		Environment.Get_Regional_Stations(Next_Station).Wait_For_Train_To_Arrive(
			Incoming_Traveler 	=> This.Traveler_Manager_Index,
			Train_ID 			=> Train_ID,
			Platform_Index		=> Destination_Platform_Index);

	exception
		when Error : others =>
			Logger.Log(
				NAME_LEAVE,
				"Exception: " & Ada.Exceptions.Exception_Name(Error) & " , " & Ada.Exceptions.Exception_Message(Error),
				Logger.ERROR
			);
    end Do_Operation;

end Move_Operation;

