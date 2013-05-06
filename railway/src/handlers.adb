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
with Regional_Ticket_Office;
with Traveler_Pool;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Move_Operation;

package body Handlers Is

-- #################################################### TRAIN ##############################################################

	procedure Station_Train_Transfer_Handler(
		Msg : in 	Incoming_Message'Class) is

		procedure Callback (Content : in out YAMI.Parameters.Parameters_Collection)
		is
			-- # First Retrieve all the parameters from the given content
			Station_Index 			: Integer	:= Integer'Value(Content.Get_String("station"));
			Platform_Index 			: Integer	:= Integer'Value(Content.Get_String("platform"));
			Train_Index				: Integer	:= Integer'Value(Content.Get_String("train_index"));
			Time_Table_Index		: Positive	:= Integer'Value(Content.Get_String("current_run"));
			Time_Table_Position		: Positive	:= Integer'Value(Content.Get_String("current_run_position"));
			Train_Data				: String 	:= Content.Get_String("train");

			Reply_Parameters 	: YAMI.Parameters.Parameters_Collection := YAMI.Parameters.Make_Parameters;

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

				Put_Line("Time_Table_Position = " & Integer'Image(Time_Table_Position));
				Put_Line("Time_Table_Index = " & Integer'Image(Time_Table_Index));


				Environment.Route_Time_Table(Trains.Trains(Train_Index).Route_Index).Current_Run_Cursor := Time_Table_Position;

				Environment.Route_Time_Table(Trains.Trains(Train_Index).Route_Index).Current_Run := Time_Table_Index;

				-- # At this point the Stage Index will have been incremented by Leave, so we are
				-- # sure that the fake stage is passed, and that at the index Next_Stage there will be
				-- # the next stage to travel.

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

				Reply_Parameters.Set_String("response",OK);

			else
				Logger.Log(
					Sender		=> "Station_Traveler_Arrive_Transfer_Handler",
					Message		=> "Invalid Station index : " & Integer'Image(Station_Index),
					L 			=> Logger.ERROR
				);

				Reply_Parameters.Set_String("response",ERROR);
				Reply_Parameters.Set_String("message","Invalid Station index : " & Integer'Image(Station_Index));

			end if;
			Msg.Reply(Reply_Parameters);
		exception
			when E : others =>
				Logger.Log(
					Sender => "",
					Message => "ERROR : Exception: " & Ada.Exceptions.Exception_Name(E) & "  " & Ada.Exceptions.Exception_Message(E),
					L => Logger.ERROR);
		end Callback;

	-- # Procedure Body
	begin

		Msg.Process_Content(Callback'Access);

	end Station_Train_Transfer_Handler;





	procedure Station_Train_Transfer_Left_Handler(
		Msg : in 	Incoming_Message'Class) is

		procedure Callback(Content : in out YAMI.Parameters.Parameters_Collection) is
			-- # First Retrieve all the parameters from the given content
			Station_Index 	: Integer	:= Integer'Value(Content.Get_String("station"));
			Platform_Index 	: Integer	:= Integer'Value(Content.Get_String("platform"));
			Train_Index		: Integer	:= Integer'Value(Content.Get_String("train_index"));

			Reply_Parameters 	: YAMI.Parameters.Parameters_Collection := YAMI.Parameters.Make_Parameters;

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

				Reply_Parameters.Set_String("response",OK);

			else
				Logger.Log(
					Sender		=> "Station_Traveler_Arrive_Transfer_Handler",
					Message		=> "Invalid Station index : " & Integer'Image(Station_Index),
					L 			=> Logger.ERROR
				);

				Reply_Parameters.Set_String("response",ERROR);

			end if;

			Msg.Reply(Reply_Parameters);

		exception
			when E : others =>
			Logger.Log(
				Sender => "",
				Message => "ERROR : Exception: " & Ada.Exceptions.Exception_Name(E) & "  " & Ada.Exceptions.Exception_Message(E),
				L => Logger.ERROR);
		end Callback;

	begin

		Msg.Process_Content(Callback'Access);

    end Station_Train_Transfer_Left_Handler;

-- #################################################### TRAVELER ##############################################################

	procedure Station_Traveler_Leave_Transfer_Handler(
		Msg : in 	Incoming_Message'Class) is

		procedure Callback(Content : in out YAMI.Parameters.Parameters_Collection) is
			-- # First Retrieve all the parameters from the given content
			Station_Index 	: Integer	:= Integer'Value(Content.Get_String("station"));
			Platform_Index 	: Integer	:= Integer'Value(Content.Get_String("platform"));
			Traveler_Index	: Integer	:= Integer'Value(Content.Get_String("traveler_index"));
			Train_ID		: Integer	:= Integer'Value(Content.Get_String("train_id"));
			Traveler_Data	: String 	:= Content.Get_String("traveler");
			Ticket_Data		: String 	:= Content.Get_String("ticket");

			Reply_Parameters 	: YAMI.Parameters.Parameters_Collection := YAMI.Parameters.Make_Parameters;

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

				Msg.Reply(Reply_Parameters);

				Reply_Parameters.Set_String("response",OK);

			else
				Logger.Log(
					Sender		=> "Station_Traveler_Arrive_Transfer_Handler",
					Message		=> "Invalid Station index : " & Integer'Image(Station_Index),
					L 			=> Logger.ERROR
				);

				Reply_Parameters.Set_String("response",ERROR);

			end if;

			Msg.Reply(Reply_Parameters);

		exception
			when E : others =>
			Logger.Log(
				Sender => "",
				Message => "ERROR : Exception: " & Ada.Exceptions.Exception_Name(E) & "  " & Ada.Exceptions.Exception_Message(E),
				L => Logger.ERROR);

		end Callback;

	begin

		Msg.Process_Content(Callback'Access);

	end Station_Traveler_Leave_Transfer_Handler;


	procedure Station_Traveler_Enter_Transfer_Handler(
		Msg : in 	Incoming_Message'Class) is

		procedure Callback(Content : in out YAMI.Parameters.Parameters_Collection) is
			-- # First Retrieve all the parameters from the given content
			Station_Index 	: Integer	:= Integer'Value(Content.Get_String("station"));
			Platform_Index 	: Integer	:= Integer'Value(Content.Get_String("platform"));
			Traveler_Index	: Integer	:= Integer'Value(Content.Get_String("traveler_index"));
			Train_ID		: Integer	:= Integer'Value(Content.Get_String("train_id"));
			Traveler_Data	: String 	:= Content.Get_String("traveler");
			Ticket_Data		: String 	:= Content.Get_String("ticket");

			Reply_Parameters 	: YAMI.Parameters.Parameters_Collection := YAMI.Parameters.Make_Parameters;

		begin
			if Station_Index <= Environment.Stations'Length then


				Put_Line("TRAVELER_DATA = " & Traveler_Data);


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

				Reply_Parameters.Set_String("response",OK);

			else
				Logger.Log(
					Sender		=> "Station_Traveler_Arrive_Transfer_Handler",
					Message		=> "Invalid Station index : " & Integer'Image(Station_Index),
					L 			=> Logger.ERROR
				);

				Reply_Parameters.Set_String("response",ERROR);

			end if;

			Msg.Reply(Reply_Parameters);

		exception
			when E : others =>
			Logger.Log(
				Sender => "",
				Message => "ERROR : Exception: " & Ada.Exceptions.Exception_Name(E) & "  " & Ada.Exceptions.Exception_Message(E),
				L => Logger.ERROR);

		end Callback;

	begin

		Msg.Process_Content(Callback'Access);

	end Station_Traveler_Enter_Transfer_Handler;

-- #################################################### TICKET CREATION ##############################################################

	procedure Get_Ticket_Handler(
		Msg : in 	Incoming_Message'Class)
	is
		procedure Callback(Content : in out YAMI.Parameters.Parameters_Collection) is

			-- # Extract the two ends of the ticket
			From	: String := Content.Get_String("from");
			To		: String := Content.Get_String("to");

			Reply_Parameters 	: YAMI.Parameters.Parameters_Collection := YAMI.Parameters.Make_Parameters;
		begin
			declare
				-- # Creates
				T : Regional_Ticket_Office.Create_Request_Result := Regional_Ticket_Office.Create_Ticket(From,To);
			begin

				-- # Send back the Created Ticket!
				Reply_Parameters.Set_String("response","RECEIVED");
				Reply_Parameters.Set_String("ticket",Ticket.To_Json(T.The_Ticket));


			exception
				-- # If an error occurs, send an ERROR message
				when E : others =>
					Logger.Log(
						Sender => "",
						Message => "ERROR : Exception: " & Ada.Exceptions.Exception_Name(E) & "  " & Ada.Exceptions.Exception_Message(E),
						L => Logger.ERROR);

				Reply_Parameters.Set_String("response",ERROR);
				Reply_Parameters.Set_String("message","No ticket from " & From & " to " & To);

			end;

			Msg.Reply(Reply_Parameters);

		end Callback;

	begin

		Msg.Process_Content(Callback'Access);

    end Get_Ticket_Handler;



    procedure Is_Station_Present_Handler(
		Msg : in 	Incoming_Message'Class)
	is
		procedure Callback(Content : in out YAMI.Parameters.Parameters_Collection) is

			Station	: String := Content.Get_String("station");

			Reply_Parameters 	: YAMI.Parameters.Parameters_Collection := YAMI.Parameters.Make_Parameters;
		begin

				if Environment.Get_Index_For_Name(Station) /= 0 then
					-- # Send back the Created Ticket!
					Reply_Parameters.Set_String("response","TRUE");
				else
					Reply_Parameters.Set_String("response","FALSE");
				end if;
				Msg.Reply(Reply_Parameters);

		exception
			-- # If an error occurs, send an ERROR message
			when E : others =>
				Logger.Log(
					Sender => "",
					Message => "ERROR : Exception: " & Ada.Exceptions.Exception_Name(E) & "  " & Ada.Exceptions.Exception_Message(E),
					L => Logger.ERROR);

		end Callback;

	begin

		Msg.Process_Content(Callback'Access);

    end Is_Station_Present_Handler;


    procedure Ticket_Ready_Handler(
    	Msg : in 	Incoming_Message'Class)
    is
		procedure Callback(Content : in out YAMI.Parameters.Parameters_Collection) is
			Response 		: String 	:= Content.Get_String("response");
			Reply_Parameters 	: YAMI.Parameters.Parameters_Collection := YAMI.Parameters.Make_Parameters;
		begin
			-- # Check if the ticket was released by the Central Office
			if Response = "OK" then
				declare
					-- # If the result is OK, there will be for sure "traveler_index" field and "ticket" field.
					Traveler_Index	: Integer 	:= Integer'Value(Content.Get_String("traveler_index"));
					Ticket_Data		: String 	:= Content.Get_String("ticket");
				begin
					-- # Retrieve the Ticket
					Environment.Travelers(Traveler_Index).The_Ticket := Ticket.Get_Ticket(Ticket_Data);

					Put_Line("***** ARRIVED TICKET : ");
					Ticket.Print(Environment.Travelers(Traveler_Index).The_Ticket);

					-- # Put TICKET_READY operation on the Pool queue
					Traveler_Pool.Execute(Environment.Operations(Traveler_Index)(Traveler.TICKET_READY));
				end;
			else
				Logger.Log(
					Sender => "",
					Message => "Ticket NOT created!",
					L => Logger.ERROR);

				-- # TODO: RETRY CON UN'ALTRA DESTINAZIONE ?

			end if;

			-- # In Any case, give a response back to the Central Ticket Office.
			Reply_Parameters.Set_String("response",OK);
			Msg.Reply(Reply_Parameters);

		exception
			-- # If an error occurs, send an ERROR message
			when E : others =>
				declare
					S : Unbounded_String := Environment.Get_Random_Destination;
				begin
					Logger.Log(
						Sender => "",
						Message => "ERROR : Exception: " & Ada.Exceptions.Exception_Name(E) & "  " & Ada.Exceptions.Exception_Message(E),
						L => Logger.ERROR);

--  					while (S = Environment.Travelers(Traveler_Index).Destination) loop
--  						Environment.Travelers(Traveler_Index).Destination := S;
--  					end loop;
				end;

		end Callback;

	begin

		Msg.Process_Content(Callback'Access);

    end Ticket_Ready_Handler;


    procedure Termination_Handler(
		Msg : in 	Incoming_Message'Class)
	is
		procedure Callback(Content : in out YAMI.Parameters.Parameters_Collection) is
			Reply_Parameters 	: YAMI.Parameters.Parameters_Collection := YAMI.Parameters.Make_Parameters;

		begin

			-- # Reply Central Controller, to notify the message have been received.
			Reply_Parameters.Set_String("response",OK);
			Msg.Reply(Reply_Parameters);

			--Traveler_Pool.Execute(new Move_Operation.Terminate_Operation_Type);

			-- # Ask to terminate to Pools
			Train_Pool.Stop;
			Traveler_Pool.Stop;

			Logger.Log(
				Sender 	=> "Termination_Handler",
				Message => "Termination Request Accepted",
				L 		=> Logger.NOTICE);

		exception
			-- # If an error occurs, send an ERROR message
			when E : others =>
				declare
				begin
					Logger.Log(
						Sender => "Termination_Handler",
						Message => "ERROR : Exception: " & Ada.Exceptions.Exception_Name(E) & "  " & Ada.Exceptions.Exception_Message(E),
						L => Logger.ERROR);
				end;

		end Callback;

	begin

		Msg.Process_Content(Callback'Access);

    end Termination_Handler;


end Handlers;
