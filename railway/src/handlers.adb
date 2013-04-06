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
with Ada.Exceptions;
with Ada.Text_IO;use Ada.Text_IO;
with Routes;
with Traveler;
with Ticket;

package body Handlers Is


	procedure Station_Train_Transfer_Handler(Content : in out YAMI.Parameters.Parameters_Collection) is

		-- # First Retrieve all the parameters from the given content
		Station_Index 	: Integer	:= Integer'Value(Content.Get_String("station"));
		Platform_Index 	: Integer	:= Integer'Value(Content.Get_String("platform"));
		Train_Index		: Integer	:= Integer'Value(Content.Get_String("train_index"));
		Train_Data		: String 	:= Content.Get_String("train");


	begin
		if Station_Index <= Environment.Get_Regional_Stations'Length then

			Trains.Update_Train(
				Train_Index 	=> Train_Index,
				Train_To_Copy	=> Train.Get_Train_Descriptor(Train_Data));

			-- # Set the current station for the Train to be the current Gateway Station
			Trains.Trains(Train_Index).Current_Station := Station_Index;

			Logger.Log(
				Sender		=> "Station_Message_Handler",
				Message		=> "Updated Train " & Train_Data,
				L 			=> Logger.DEBUG
			);

			--
			-- # NEED A PROCEDURE TO OCCUPY THE SAME PLATFORM AS AT THE OTHER SIDE!!!!
			--

			Environment.Get_Regional_Stations(Station_Index).Leave(
				Descriptor_Index	=> Train_Index,
				Platform_Index 		=> Platform_Index
			);

			-- # At this point the Stage Index will have been incremented by Leave, so we are
			-- # sure that the fake stage is passed, and that at the index Next_Stage there will be
			-- # the next stage to travel.

			Put_Line("Next_Stage = " & Integer'Image(Trains.Trains(Train_Index).Next_Stage));

			-- # Re-enqueue the descriptor only if it has more stages to travel
			if(Trains.Trains(Train_Index).Next_Stage <= Routes.All_Routes(Trains.Trains(Train_Index).Route_Index)'Length) then
				Train_Pool.Associate(Train_Index);
			else
				Logger.Log(
					Sender		=> "Station_Message_Handler",
			      	Message 	=> "Train" & Integer'Image(Trains.Trains(Train_Index).Id) & " finished its run!",
			      	L			=> Logger.DEBUG);
			end if;

		end if;
	exception
		when E : others =>
		Logger.Log(
			Sender => "",
			Message => "ERROR : Exception: " & Ada.Exceptions.Exception_Name(E) & "  " & Ada.Exceptions.Exception_Message(E),
			L => Logger.ERROR);


	end Station_Train_Transfer_Handler;


	procedure Station_Traveler_Leave_Transfer_Handler(Content : in out YAMI.Parameters.Parameters_Collection) is

		-- # First Retrieve all the parameters from the given content
		Station_Index 	: Integer	:= Integer'Value(Content.Get_String("station"));
		Platform_Index 	: Integer	:= Integer'Value(Content.Get_String("platform"));
		Traveler_Index	: Integer	:= Integer'Value(Content.Get_String("traveler_index"));
		Traveler_Data	: String 	:= Content.Get_String("traveler");
		Ticket_Data		: String 	:= Content.Get_String("ticket");

	begin
		if Station_Index <= Environment.Get_Regional_Stations'Length then

			Environment.Update_Traveler(
				Traveler_Index 	=> Traveler_Index,
				Trav_To_Copy	=> Traveler.Get_Traveler_Manager(Traveler_Data),
				Ticket_To_Copy  => Ticket.Get_Ticket(Ticket_Data));

			Logger.Log(
				Sender		=> "Station_Message_Handler",
				Message		=> "Updated Traveler : " & Traveler_Data & " Ticket : " & Ticket_Data,
				L 			=> Logger.DEBUG
			);


		end if;
	exception
		when E : others =>
		Logger.Log(
			Sender => "",
			Message => "ERROR : Exception: " & Ada.Exceptions.Exception_Name(E) & "  " & Ada.Exceptions.Exception_Message(E),
			L => Logger.ERROR);


	end Station_Traveler_Leave_Transfer_Handler;


end Handlers;
