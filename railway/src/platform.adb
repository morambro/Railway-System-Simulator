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
with Ada.Containers;use Ada.Containers;
with Logger;
with Ada.Strings.Unbounded;use Ada.Strings.Unbounded;
with Environment;
with Ada.Text_IO;use Ada.Text_IO;
with Traveler_Pool;
with Ada.Exceptions;
with Trains;
with Traveler;
with Routes;
with Central_Controller_Interface;


with Ada.Calendar;
with Ada.Calendar.Formatting;use Ada.Calendar.Formatting;
with Ada.Calendar.Time_Zones;use Ada.Calendar.Time_Zones;


package body Platform is

	NAME : constant String := "Platfrom.Platform_Type";

	protected body Platform_Type is

		procedure Leave
		is
			Leaving_Train : Integer;
		begin
			-- # It simply Frees the Platform to let other Trains access it
			Trains_Order.Dequeue(Leaving_Train);

			if Retry'Count > 0 then
				Retry_Count := Retry'Count;
				Can_Retry := True;
			end if;

		end Leave;

		procedure Terminate_Platform is
		begin
			Terminated := True;
		end Terminate_Platform;

		entry Retry (
			Train_Descriptor_Index 	: in 	Positive) when Can_Retry or Terminated is
		begin
			if Terminated then
				Logger.Log(
					Sender 	=> "Platform_Type",
					Message => "Termination Requested",
					L 		=> Logger.INFO);
			else
				Retry_Count := Retry_Count - 1;

				if Retry_Count = 0 then
					Can_Retry := False;
				end if;

				requeue Enter;
			end if;
		end Retry;

		entry Enter (
			Train_Descriptor_Index 	: in 	Positive) when True is
		begin
			if Trains_Order.Get(1) /= Trains.Trains(Train_Descriptor_Index).ID then
				requeue Retry;
			end if;
		end Enter;

		procedure Add_Train(
			Train_Index : in 	Positive)is
		begin
			Trains_Order.Enqueue(Trains.Trains(Train_Index).ID);
		end Add_Train;

	end Platform_Type;


	-- ################################## PLATFORM_HANDLER #########################################


	procedure Add_Train(
		This					: access Platform_Handler;
		Train_Index				: in 	 Positive) is
	begin
		Put_Line(integer'image(Train_Index));
		This.The_Platform.Add_Train(Train_Index);
    end Add_Train;


	procedure Perform_Entrance(
		This 					: access Platform_Handler;
		Train_Descriptor_Index 	: in 	Positive;
		Action 					: in	Route.Action)
	is
		Arrival_Number 			: Count_Type := This.Arrival_Queue.Current_Use;
		Traveler_Manager_Index	: Positive;
		Next_Stage 				: Positive;
	begin
		-- #
		-- # Alighting of Travelers
		-- #
		if Action = Route.ENTER then
			Logger.Log(
					Sender  => NAME,
					Message => "Train " & Integer'Image(Trains.Trains(Train_Descriptor_Index).Id) & " Performs Alighting of travelers" &
								" platform " & Integer'Image(This.ID),
					L       => Logger.DEBUG);

			for I in 1..Arrival_Number loop
				-- # in Traveler_Manager_Index there will be the index of the next Traveler
				This.Arrival_Queue.Dequeue(Traveler_Manager_Index);

				-- # Retrieve the next stage from the ticket of the current Traveler
				Next_Stage := Environment.Travelers(Traveler_Manager_Index).The_Ticket.Next_Stage;
				if Environment.Travelers(Traveler_Manager_Index).The_Ticket.Stages(Next_Stage).Train_ID /= Trains.Trains(Train_Descriptor_Index).Id then
					-- # If the current Traveler was not waiting for this train, re-queue it
					This.Arrival_Queue.Enqueue(Traveler_Manager_Index);
				else

					-- # Decrease the number of occupied sits
					if Trains.Trains(Train_Descriptor_Index).Occupied_Sits > 0 then
						Trains.Trains(Train_Descriptor_Index).Occupied_Sits :=
							Trains.Trains(Train_Descriptor_Index).Occupied_Sits - 1;
					else
						Logger.Log(
							Sender 	=> NAME,
							Message => 	"ERROR : Traveler " & Integer'Image(Traveler_Manager_Index) &
										" arrived without traveling!",
							L		=> Logger.ERROR
						);
					end if;

					Logger.Log(
						Sender  => NAME,
						Message => "Traveler " &
									Integer'Image(Environment.Travelers(Traveler_Manager_Index).Traveler.ID) &
								   " Leaves the train at station " &
								   Integer'Image(Environment.Travelers(Traveler_Manager_Index).The_Ticket.Stages(Next_Stage).Next_Station),
						L       => Logger.DEBUG);


					-- # Now let's check the status of the Travel for the Traveler
					declare
						-- # Next operation to execute.
						Next_Operation : Traveler.Traveler_Operations_Types := Traveler.LEAVE;
					begin

						-- # Check if there are more stages, otherwise re-start
						if 	Environment.Travelers(Traveler_Manager_Index).The_Ticket.Next_Stage =
							Environment.Travelers(Traveler_Manager_Index).The_Ticket.Stages'Length then

							Logger.Log(
								Sender  => NAME,
								Message => "Traveler " &
											Integer'Image(Environment.Travelers(Traveler_Manager_Index).Traveler.ID) &
										   " FINISHED HIS TRAVEL" &
										   Integer'Image(Environment.Travelers(Traveler_Manager_Index).The_Ticket.Stages(Next_Stage).Next_Station),
								L       => Logger.DEBUG);

							-- # Notify Central Controller the Traveler Arrived!
							Central_Controller_Interface.Set_Traveler_Finished_Status(
								Traveler	=> Traveler_Manager_Index,
								Train		=> Trains.Trains(Train_Descriptor_Index).Id,
								Station		=> Environment.Stations(Environment.Travelers(Traveler_Manager_Index).The_Ticket.Stages(Next_Stage).Next_Station).Get_Name,
								Platform	=> This.ID);

							-- # Time to come back, switch start and destination stations!
							declare
								To_Switch : Unbounded_String := Environment.Travelers(Traveler_Manager_Index).Destination;
							begin
								-- # First switch fixed Start and Destination
								Environment.Travelers(Traveler_Manager_Index).Destination :=
										Environment.Travelers(Traveler_Manager_Index).Start_Station;

								Environment.Travelers(Traveler_Manager_Index).Start_Station := To_Switch;

								-- #.. Then update Current_Start_Stations and Current_Dest_Station
								Environment.Travelers(Traveler_Manager_Index).Current_Start_Station :=
										Environment.Travelers(Traveler_Manager_Index).Destination;
								Environment.Travelers(Traveler_Manager_Index).Current_Dest_Station 	:=
										Environment.Travelers(Traveler_Manager_Index).Start_Station;
							end;

							-- # Next operation tells Traveler to buy a ticket!
							Next_Operation := Traveler.BUY_TICKET;

						else
							-- # Go to the next stage (there will be at least another one for sure!)
							Environment.Travelers(Traveler_Manager_Index).The_Ticket.Next_Stage :=
								Environment.Travelers(Traveler_Manager_Index).The_Ticket.Next_Stage + 1;
						end if;

						-- # Execute the operation Next_Operation
						Traveler_Pool.Execute(Environment.Operations(Traveler_Manager_Index)(Next_Operation));

					exception
						when Error : others =>
							Logger.Log(
								Sender  => NAME,
							    Message => "EXCEPTION: " & Ada.Exceptions.Exception_Name(Error) & " , " &
							    			Ada.Exceptions.Exception_Message(Error),
							    L       => Logger.ERROR);
					end;
				end if;
			end loop;
		end if;
    end Perform_Entrance;

	procedure Perform_Exit(
			This 					: access Platform_Handler;
			Train_Descriptor_Index 	: in 	Positive;
			Action 					: in	Route.Action)
	is
		Leaving_Number 			: Count_Type := This.Leaving_Queue.Current_Use;
		Traveler_Manager_Index	: Positive;
		Next_Stage 				: Positive;
	begin

		case Action is

		when Route.ENTER =>
			-- #
			-- # Boarding of Travelers
			-- #
			Logger.Log(
					Sender  => NAME,
					Message => 	"Train " & Integer'Image(Trains.Trains(Train_Descriptor_Index).Id) &
								" Performs Boarding of travelers",
					L       => Logger.DEBUG);
			for I in 1..Leaving_Number loop

				-- # Retrieve the next Traveler Manager Index
				This.Leaving_Queue.Dequeue(Traveler_Manager_Index);


				-- # Retrieve the Next_Stage Index
				Next_Stage := Environment.Travelers(Traveler_Manager_Index).The_Ticket.Next_Stage;

				-- # Check if the Current Train is the one the Traveler is waiting for.
				if Environment.Travelers(Traveler_Manager_Index).The_Ticket.Stages(Next_Stage).Train_ID /= Trains.Trains(Train_Descriptor_Index).Id then
					declare
						-- # Current Run for witch the Ticket have been booked.
						Current_Run_Id 	: Natural := Environment.Travelers(Traveler_Manager_Index).The_Ticket.Stages(Next_Stage).Current_Run_Id;
						-- # The last run for the waited Train
						Last_Run 		: Natural := This.Get_Run_For_Train(Environment.Travelers(Traveler_Manager_Index).The_Ticket.Stages(Next_Stage).Train_ID);
					begin
						-- # If the Train have already passed the platform.
						if Current_Run_Id > 0 and Current_Run_Id <= Last_Run then
							-- # Make user buy another Ticket!!
							Logger.log(
								Sender 	=> "Platform",
								Message => "Traveler " & Integer'Image(Traveler_Manager_Index) & " lost the booked Train; buy a new Ticket!",
								L		=> Logger.ERROR);
							-- # Set Current Station as Start Station
							Environment.Travelers(Traveler_Manager_Index).Current_Start_Station := This.Station_Name.all;
							Traveler_Pool.Execute(Environment.Operations(Traveler_Manager_Index)(Traveler.BUY_TICKET));
						else
							-- # If the current Traveler was not waiting for this train, re-queue it
							This.Leaving_Queue.Enqueue(Traveler_Manager_Index);
						end if;
						null;
					end;
				else
					-- # If the next ticket stage contains a booked train, and the current train's run is NOT the same
					-- # ad the next stage's run, the Traveler lost the Train!
					if 	Environment.Travelers(Traveler_Manager_Index).The_Ticket.Stages(Next_Stage).Current_Run_Id > 0 and
						This.Get_Run_For_Train(Trains.Trains(Train_Descriptor_Index).Id) >
						Environment.Travelers(Traveler_Manager_Index).The_Ticket.Stages(Next_Stage).Current_Run_Id then

						Logger.log(
							Sender 	=> "Platform",
							Message => 	"Traveler " & Integer'Image(Traveler_Manager_Index) &
										" lost the booked Train; buy a new Ticket!",
							L		=> Logger.ERROR);

						-- # Set the current Station as the start station
						Environment.Travelers(Traveler_Manager_Index).Current_Start_Station := This.Station_Name.all;

						-- # Set BUY_TICKET Operation to make him buy a new Ticket
						Traveler_Pool.Execute(Environment.Operations(Traveler_Manager_Index)(Traveler.BUY_TICKET));

					else
						-- # Increase the number of occupied sits
						Trains.Trains(Train_Descriptor_Index).Occupied_Sits := Trains.Trains(Train_Descriptor_Index).Occupied_Sits + 1;

						-- # If the current traveler have to board to the train...
						Logger.Log(
							Sender  => NAME,
							Message => "Traveler " &
									   Integer'Image(Environment.Travelers(Traveler_Manager_Index).Traveler.ID) &
									   " boarding at station " &
									   Integer'Image(Environment.Travelers(Traveler_Manager_Index).The_Ticket.Stages(Next_Stage).Next_Station),
							L       => Logger.DEBUG);


						declare
							-- # The next Traveler operation
							Next_Operation : Traveler.Traveler_Operations_Types := Traveler.ENTER;
						begin
							-- # Execute the operation ENTER (Traveler waits to leave the train).
							Environment.Operations(Traveler_Manager_Index)(Next_Operation).Do_Operation;

						exception
							when Error : others =>
								Logger.Log(
									Sender  => NAME,
								    Message => "EXCEPTION: " & Ada.Exceptions.Exception_Name(Error) & " , " &
								    			Ada.Exceptions.Exception_Message(Error),
								    L       => Logger.ERROR);

						end;
					end if;
				end if;
			end loop;

			-- # Update the Map with the current Run ID!
			if This.Train_Run.Contains(Trains.Trains(Train_Descriptor_Index).Id) then
				This.Train_Run.Replace(
					Trains.Trains(Train_Descriptor_Index).Id,
					Environment.Route_Time_Table(Trains.Trains(Train_Descriptor_Index).Route_Index).Current_Run_Id);
			else
				This.Train_Run.Insert(
					Trains.Trains(Train_Descriptor_Index).Id,
					Environment.Route_Time_Table(Trains.Trains(Train_Descriptor_Index).Route_Index).Current_Run_Id);
			end if;

			Put_Line("Train " & Integer'Image(Trains.Trains(Train_Descriptor_Index).Id) & " run_id = " &
					Integer'Image(Environment.Route_Time_Table(Trains.Trains(Train_Descriptor_Index).Route_Index).Current_Run_Id));

			Logger.Log(
				Sender  => NAME,
				Message => "Train " &
						   Integer'Image(Trains.Trains(Train_Descriptor_Index).Id) &
						   " has" & Integer'Image(Trains.Trains(Train_Descriptor_Index).Occupied_Sits) & "/" &
						   Integer'Image(Trains.Trains(Train_Descriptor_Index).Sits_Number) & " travelers",
				L       => Logger.DEBUG);
		when  Route.PASS =>
				Put_Line("ACTION = PASS");
				Logger.Log(
					Sender  => NAME,
					Message => "Train " &
							   Integer'Image(Trains.Trains(Train_Descriptor_Index).Id) &
							   " passes away",
					L       => Logger.DEBUG);

		when  Route.FREE =>
			Put_Line("ACTION = FREE");
			Logger.Log(
				Sender  => NAME,
				Message => "Train " &
						   Integer'Image(Trains.Trains(Train_Descriptor_Index).Id) &
						   " freed the Platform",
				L       => Logger.DEBUG);
    	end case;
    end Perform_Exit;


	procedure Enter(
		This 					: access Platform_Handler;
		Train_Descriptor_Index 	: in 	 Positive;
		Action 					: in	 Route.Action) is
	begin

		This.The_Platform.Enter(
				Train_Descriptor_Index 	=> Train_Descriptor_Index);

		declare
			-- # Get the Current Run index
			Current_Run 	: Positive :=
				Environment.Route_Time_Table(Trains.Trains(Train_Descriptor_Index).Route_Index).Current_Run;

			-- # Get the index of the next time to leave the station
			Current_Run_Cursor	: Positive :=
				Environment.Route_Time_Table(Trains.Trains(Train_Descriptor_Index).Route_Index).Current_Run_Cursor;
			-- # Time to wait before leaving
			Time_To_Wait : Ada.Calendar.Time := Environment.Route_Time_Table(Trains.Trains(Train_Descriptor_Index).Route_Index).Table
				(Current_Run)(Current_Run_Cursor);

			Train_Delay : Duration := Ada.Calendar."-"(Ada.Calendar.Clock, Time_To_Wait);
		begin

			-- # At this point the Task Train has access to the platform,
			-- # So notify the Central Controller.
			Central_Controller_Interface.Set_Train_Arrived_Status(
				Train_ID	=> Trains.Trains(Train_Descriptor_Index).ID,
				Station		=> To_String(This.Station_Name.all),
				Platform	=> This.ID,
				-- # Time at witch the Train will leave the Platform.
				Time 		=> Ada.Calendar.Formatting.Image(
								Date					=> Time_To_Wait,
								Include_Time_Fraction 	=> False,
								-- # We are 2 hours later that UTC Time Zone.
								Time_Zone				=> 2*60),
				-- # Duration rounded to Integer, representing seconds
				-- # of delay.
				Train_Delay	=> Integer(Train_Delay));
		end;

		-- # CODE FOR ENTRANCE
		This.Perform_Entrance(
			Train_Descriptor_Index	=> Train_Descriptor_Index,
			Action 					=> Action);

    end Enter;

	procedure Leave(
		This 					: access Platform_Handler;
		Train_Descriptor_Index 	: in 	Positive;
		Action 					: in	Route.Action) is
	begin

		-- # Let the Train leave the platform
		This.Perform_Exit(
			Train_Descriptor_Index	=> Train_Descriptor_Index,
			Action					=> Action);

		-- # Then set to Free the protected resource
		This.The_Platform.Leave;
    end Leave;

	procedure Add_Incoming_Traveler(
		This 					: access Platform_Handler;
		Traveler 				: in 	Positive) is
	begin
		-- # Simply adds the given Traveler to the queue (in mutual exclusion,
		-- # since the used queue is synchronized!)
		This.Arrival_Queue.Enqueue(Traveler);
    end Add_Incoming_Traveler;

	procedure Add_Outgoing_Traveler(
		This 					: access Platform_Handler;
		Traveler 				: in 	Positive) is
	begin
		-- # Simply adds the given Traveler to the queue (in mutual exclusion,
		-- # since the used queue is synchronized!)
		This.Leaving_Queue.Enqueue(Traveler);
    end Add_Outgoing_Traveler;


    procedure Terminate_Platform(
		This 				: access Platform_Handler) is
	begin
		This.The_Platform.Terminate_Platform;
	end Terminate_Platform;


	procedure Init(
		This					: access Platform_Handler;
		Station_Name			: access Unbounded_String;
		Notice_Panel_Ref		: access Notice_Panel.Notice_Panel_Entity;
		ID						: in	 Positive) is
	begin
		This.Station_Name 	:= Station_Name;
		This.Panel			:= Notice_Panel_Ref;
		This.ID				:= ID;
    end Init;

	function Get_Run_For_Train(
		This 					: access Platform_Handler;
		Train_ID				: in	 Integer) return Integer is
	begin
		if This.Train_Run.Contains(Train_ID) then
			return This.Train_Run.Element(Train_ID);
		end if;
		-- # If the given Train_ID did not passed yet
		return 0;
    end Get_Run_For_Train;


end Platform;
