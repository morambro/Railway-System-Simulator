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
with Central_Controller_Interface;
with Ada.Strings.Unbounded;
with Environment;
with Logger;
with Ada.Exceptions;
with Ticket;
with Traveler_Pool;
with Remote_Station_Interface;
with Ada.Numerics.Discrete_Random;
with Train_Pool;
with Message_Agent;

package body Move_Operation is

	-- # Range for random numbers
	type Random_Range is range 3..10;

	package Rand_Int is new Ada.Numerics.Discrete_Random(Random_Range);

	-- # Seed and number
	Seed 	: Rand_Int.Generator;
	Num 	: Random_Range;

	use Ada.Strings.Unbounded;

	procedure Do_Operation(This : in Leave_Operation_Type) is
		Next_Stage 				: Positive 	:= Environment.Travelers(This.Traveler_Manager_Index).The_Ticket.Next_Stage;
		Start_Station 			: Natural 	:= Environment.Travelers(This.Traveler_Manager_Index).The_Ticket.Stages(Next_Stage).Start_Station;
		Train_ID	 			: Natural 	:= Environment.Travelers(This.Traveler_Manager_Index).The_Ticket.Stages(Next_Stage).Train_ID;
		Start_Platform_Index 	: Natural 	:= Environment.Travelers(This.Traveler_Manager_Index).The_Ticket.Stages(Next_Stage).Start_Platform_Index;
	begin
		Logger.Log(
			Sender 	=> "Move_Operation.Leave_Operation_Type",
			Message => "Traveler" & Integer'Image(Environment.Travelers(This.Traveler_Manager_Index).Traveler.ID) &
					   " will wait to LEAVE at platform" & Integer'Image(Start_Platform_Index) &
					   ", station" & Integer'Image(Start_Station) & " for train " & Integer'Image(Train_ID),
			L 		=> Logger.NOTICE);

		-- # Make the Traveler wait to a specific Train.
		Environment.Stations(Start_Station).Wait_For_Train_To_Go(
			Outgoing_Traveler 	=> This.Traveler_Manager_Index,
			Train_ID 			=> Train_ID,
			Platform_Index		=> Start_Platform_Index);

		-- # Notify the Central Controller that the current Traveler is waiting by the
		-- # platform Start_Platform_Index, station Start_Station, to catch train Train_ID
		Central_Controller_Interface.Set_Traveler_Left_Status(
			Traveler	=> This.Traveler_Manager_Index,
			Train		=> Train_ID,
			Station		=> Environment.Stations(Start_Station).Get_Name,
			Platform	=> Start_Platform_Index);


	exception
		when Error : others =>
			Logger.Log(
				"Move_Operation.Leave_Operation_Type",
				"Exception: " & Ada.Exceptions.Exception_Name(Error) & " , " & Ada.Exceptions.Exception_Message(Error),
				Logger.ERROR
			);
	end Do_Operation;


	-- #
	-- # Operation which lets the Traveler (Manager)
	-- #
	procedure Do_Operation(This : in Enter_Operation_Type) is
		Next_Stage 					: Positive 			:= Environment.Travelers(This.Traveler_Manager_Index).The_Ticket.Next_Stage;
		Next_Station 				: Natural 			:= Environment.Travelers(This.Traveler_Manager_Index).The_Ticket.Stages(Next_Stage).Next_Station;
		Next_Region 				: Unbounded_String 	:= Environment.Travelers(This.Traveler_Manager_Index).The_Ticket.Stages(Next_Stage).Region;
		Train_ID	 				: Natural 			:= Environment.Travelers(This.Traveler_Manager_Index).The_Ticket.Stages(Next_Stage).Train_ID;
		Destination_Platform_Index 	: Natural 			:= Environment.Travelers(This.Traveler_Manager_Index).The_Ticket.Stages(Next_Stage).Destination_Platform_Index;
	begin
		Logger.Log(
			Sender 	=> "Move_Operation.Enter_Operation_Type",
			Message => "Traveler" & Integer'Image(Environment.Travelers(This.Traveler_Manager_Index).Traveler.ID) &
					   " will wait to ARRIVE at platform" & Integer'Image(Destination_Platform_Index) & ", station " & Integer'Image(Next_Station),
			L 		=> Logger.NOTICE);

		-- # Check if the next destination is in the current Region or not
		if Next_Region = Environment.Get_Node_Name then

			Environment.Stations(Next_Station).Wait_For_Train_To_Arrive(
				Incoming_Traveler 	=> This.Traveler_Manager_Index,
				Train_ID 			=> Train_ID,
				Platform_Index		=> Destination_Platform_Index);

			-- # Notify the Central Controller that the current Traveler is waiting by the
			-- # platform Start_Platform_Index, station Start_Station, to leave the Train Train_ID.
			Central_Controller_Interface.Set_Traveler_Entering_Status(
				Traveler	=> This.Traveler_Manager_Index,
				Train		=> Train_ID,
				Station		=> Environment.Stations(Next_Station).Get_Name,
				Platform	=> Destination_Platform_Index);
		else
			-- # If the Next Station to access is on another station, let's make a remote call
			Remote_Station_Interface.Wait_For_Train_To_Arrive(
				Next_Station 				=> Next_Station,
				Traveler_Manager_Index		=> This.Traveler_Manager_Index,
				Train_ID					=> Train_ID,
				Destination_Platform_Index	=> Destination_Platform_Index,
				Next_Region					=> To_String(Next_Region));
		end if;

	exception
		when Error : others =>
			Logger.Log(
				"Move_Operation.Enter_Operation_Type",
				"Exception: " & Ada.Exceptions.Exception_Name(Error) & " , " & Ada.Exceptions.Exception_Message(Error),
				Logger.ERROR
			);
    end Do_Operation;


    procedure Do_Operation(This : in Buy_Ticket_Operation_Type) is
   		Start_Station : Integer := Environment.Get_Index_For_Name(To_String(Environment.Travelers(This.Traveler_Manager_Index).Current_Start_Station));
    begin

		Rand_Int.Reset(Seed);

		Num := Rand_Int.Random(Seed);

		Logger.Log(
			Sender	=> 	"Move_Operation.Buy_Ticket_Operation_Type",
			Message => 	"Traveler" & Integer'Image(This.Traveler_Manager_Index) & " will wait for" & Random_Range'image(Num) &
						" seconds before asking for a new Ticket from " & To_String(Environment.Travelers(This.Traveler_Manager_Index).Current_Start_Station) &
						" to " & To_String(Environment.Travelers(This.Traveler_Manager_Index).Current_Dest_Station),
			L 		=> 	Logger.INFO);

		Put_Line("Station index = " & Integer'image(Start_Station));

		-- # Update the Status of the Traveler
		Central_Controller_Interface.Set_Traveler_Buying_Status(
			Traveler	=> This.Traveler_Manager_Index,
			Station		=> Environment.Stations(Start_Station).Get_Name);

		-- # Wait a random amount of time between 3 and 10 seconds
		delay Duration(Num);

    	-- # Buy a Ticket at the Station Ticket Office.
		Environment.Stations(Start_Station).Buy_Ticket(
			Traveler_Index	=> This.Traveler_Manager_Index,
			To 				=> To_String(Environment.Travelers(This.Traveler_Manager_Index).Current_Dest_Station));

	exception
		when Error : others =>
			Logger.Log(
				"Move_Operation.Buy_Ticket_Operation_Type",
				"Exception: " & Ada.Exceptions.Exception_Name(Error) & " , " & Ada.Exceptions.Exception_Message(Error),
				Logger.ERROR
			);

    end Do_Operation;


    procedure Do_Operation(This : in Ticket_Ready_Operation_Type) is
    begin
    	-- # Once the ticket is ready, let's start the travel!
    	Traveler_Pool.Execute(Environment.Operations(This.Traveler_Manager_Index)(Traveler.LEAVE));
    end Do_Operation;

end Move_Operation;

