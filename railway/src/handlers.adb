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
with Gateway_Station;
with Route;

package body Handlers Is

-- #################################################### TRAIN ##############################################################

	procedure Station_Train_Transfer_Handler(Content : in out YAMI.Parameters.Parameters_Collection) is

		-- # First Retrieve all the parameters from the given content
		Station_Index 	: Integer	:= Integer'Value(Content.Get_String("station"));
		Platform_Index 	: Integer	:= Integer'Value(Content.Get_String("platform"));
		Train_Index		: Integer	:= Integer'Value(Content.Get_String("train_index"));
		Train_Data		: String 	:= Content.Get_String("train");


	begin
		if Station_Index <= Environment.Stations'Length then

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


			-- # At this point the Stage Index will have been incremented by Leave, so we are
			-- # sure that the fake stage is passed, and that at the index Next_Stage there will be
			-- # the next stage to travel.

			Put_Line("Next_Stage = " & Integer'Image(Trains.Trains(Train_Index).Next_Stage));

			-- # Re-enqueue the descriptor only if it has more stages to travel
			if(Trains.Trains(Train_Index).Next_Stage <= Routes.All_Routes(Trains.Trains(Train_Index).Route_Index)'Length) then

				-- # First set Occupied the platform.
				Gateway_Station.Gateway_Station_Type(Environment.Stations(Station_Index).all).Occupy_Platform(
					Platform_Index 	=> Platform_Index,
					Train_Index 	=> Train_Index);

				Train_Pool.Associate(Train_Index);
			else
				Logger.Log(
					Sender		=> "Station_Message_Handler",
			      	Message 	=> "Train" & Integer'Image(Trains.Trains(Train_Index).Id) & " finished its run!",
			      	L			=> Logger.DEBUG);
			end if;

		else
			Logger.Log(
				Sender		=> "Station_Traveler_Arrive_Transfer_Handler",
				Message		=> "Invalid Station index : " & Integer'Image(Station_Index),
				L 			=> Logger.ERROR
			);
		end if;
	exception
		when E : others =>
		Logger.Log(
			Sender => "",
			Message => "ERROR : Exception: " & Ada.Exceptions.Exception_Name(E) & "  " & Ada.Exceptions.Exception_Message(E),
			L => Logger.ERROR);


	end Station_Train_Transfer_Handler;


	procedure Station_Train_Transfer_Ack_Handler(Content : in out YAMI.Parameters.Parameters_Collection) is
		-- # First Retrieve all the parameters from the given content
		Station_Index 	: Integer	:= Integer'Value(Content.Get_String("station"));
		Platform_Index 	: Integer	:= Integer'Value(Content.Get_String("platform"));
		Train_Index		: Integer	:= Integer'Value(Content.Get_String("train_index"));
	begin
		-- # Check the given Station Index
		if Station_Index <= Environment.Stations'Length then
			Logger.Log(
				Sender		=> "Station_Train_Transfer_Ack_Handler",
				Message		=> "Freeing platform " & Integer'Image(Platform_Index) & " at station " & Integer'Image(Station_Index),
				L 			=> Logger.DEBUG
			);

			-- # Free the current Platform to let other Trains enter the Platform
			Environment.Stations(Station_Index).Leave(
				Descriptor_Index 		=>	Train_Index,
				Platform_Index			=> 	Platform_Index,
				Action					=> 	Route.FREE);

		else
			Logger.Log(
				Sender		=> "Station_Traveler_Arrive_Transfer_Handler",
				Message		=> "Invalid Station index : " & Integer'Image(Station_Index),
				L 			=> Logger.ERROR
			);
		end if;
	exception
		when E : others =>
		Logger.Log(
			Sender => "",
			Message => "ERROR : Exception: " & Ada.Exceptions.Exception_Name(E) & "  " & Ada.Exceptions.Exception_Message(E),
			L => Logger.ERROR);
    end Station_Train_Transfer_Ack_Handler;

-- #################################################### TRAVELER ##############################################################

	procedure Station_Traveler_Leave_Transfer_Handler(Content : in out YAMI.Parameters.Parameters_Collection) is

		-- # First Retrieve all the parameters from the given content
		Station_Index 	: Integer	:= Integer'Value(Content.Get_String("station"));
		Platform_Index 	: Integer	:= Integer'Value(Content.Get_String("platform"));
		Traveler_Index	: Integer	:= Integer'Value(Content.Get_String("traveler_index"));
		Train_ID		: Integer	:= Integer'Value(Content.Get_String("train_id"));
		Traveler_Data	: String 	:= Content.Get_String("traveler");
		Ticket_Data		: String 	:= Content.Get_String("ticket");

	begin
		if Station_Index <= Environment.Stations'Length then

			Environment.Update_Traveler(
				Traveler_Index 	=> Traveler_Index,
				Trav_To_Copy	=> Traveler.Get_Traveler_Manager(Traveler_Data),
				Ticket_To_Copy  => Ticket.Get_Ticket(Ticket_Data));

			Logger.Log(
				Sender		=> "Station_Traveler_Leave_Transfer_Handler",
				Message		=> "Updated Traveler : " & Traveler_Data & " Ticket : " & Ticket_Data,
				L 			=> Logger.DEBUG
			);

			Put_Line("Station : " & Integer'Image(Station_Index));

			Environment.Stations(Station_Index).Wait_For_Train_To_Go(
				Outgoing_Traveler 	=> Traveler_Index,
				Train_ID 			=> Train_ID,
				Platform_Index		=> Platform_Index);

		else
			Logger.Log(
				Sender		=> "Station_Traveler_Arrive_Transfer_Handler",
				Message		=> "Invalid Station index : " & Integer'Image(Station_Index),
				L 			=> Logger.ERROR
			);

		end if;
	exception
		when E : others =>
		Logger.Log(
			Sender => "",
			Message => "ERROR : Exception: " & Ada.Exceptions.Exception_Name(E) & "  " & Ada.Exceptions.Exception_Message(E),
			L => Logger.ERROR);


	end Station_Traveler_Leave_Transfer_Handler;


	procedure Station_Traveler_Enter_Transfer_Handler(Content : in out YAMI.Parameters.Parameters_Collection) is

		-- # First Retrieve all the parameters from the given content
		Station_Index 	: Integer	:= Integer'Value(Content.Get_String("station"));
		Platform_Index 	: Integer	:= Integer'Value(Content.Get_String("platform"));
		Traveler_Index	: Integer	:= Integer'Value(Content.Get_String("traveler_index"));
		Train_ID		: Integer	:= Integer'Value(Content.Get_String("train_id"));
		Traveler_Data	: String 	:= Content.Get_String("traveler");
		Ticket_Data		: String 	:= Content.Get_String("ticket");

	begin
		if Station_Index <= Environment.Stations'Length then

			Environment.Update_Traveler(
				Traveler_Index 	=> Traveler_Index,
				Trav_To_Copy	=> Traveler.Get_Traveler_Manager(Traveler_Data),
				Ticket_To_Copy  => Ticket.Get_Ticket(Ticket_Data));

			Logger.Log(
				Sender		=> "Station_Traveler_Arrive_Transfer_Handler",
				Message		=> "Updated Traveler : " & Traveler_Data & " Ticket : " & Ticket_Data,
				L 			=> Logger.DEBUG
			);

			Put_Line("Station : " & Integer'Image(Station_Index));

			Environment.Stations(Station_Index).Wait_For_Train_To_Arrive(
				Incoming_Traveler 	=> Traveler_Index,
				Train_ID 			=> Train_ID,
				Platform_Index		=> Platform_Index);

		else
			Logger.Log(
				Sender		=> "Station_Traveler_Arrive_Transfer_Handler",
				Message		=> "Invalid Station index : " & Integer'Image(Station_Index),
				L 			=> Logger.ERROR
			);
		end if;
	exception
		when E : others =>
		Logger.Log(
			Sender => "",
			Message => "ERROR : Exception: " & Ada.Exceptions.Exception_Name(E) & "  " & Ada.Exceptions.Exception_Message(E),
			L => Logger.ERROR);


	end Station_Traveler_Enter_Transfer_Handler;


end Handlers;
