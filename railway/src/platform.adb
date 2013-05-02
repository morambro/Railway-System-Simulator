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

package body Platform is

	NAME : constant String := "Platfrom.Platform_Type";

	protected body Platform_Type is

		procedure Leave(
			Train_Descriptor_Index 	: in 	Positive) is
		begin
			-- # It simply Frees the Platform to let other Trains access it
			Free := True;
		end Leave;

		entry Enter_Regional (
			Train_Descriptor_Index 	: in 	Positive) when Free and Enter_FB'Count = 0 is
		begin
			-- # Occupy the platform. Here the requirement is that no FB Trains are still trying to access the Platform
			Free := False;
		end Enter_Regional;

		entry Enter_FB (
			Train_Descriptor_Index 	: in 	Positive) when Free is
		begin
			-- # Occupy the platform. The requirement is Platform Free.
			Free := False;
		end Enter_FB;

	end Platform_Type;


	-- ################################## PLATFORM_HANDLER #########################################


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
						Trains.Trains(Train_Descriptor_Index).Occupied_Sits := Trains.Trains(Train_Descriptor_Index).Occupied_Sits - 1;
					else
						Logger.Log(
							Sender 	=> NAME,
							Message => "ERROR : Traveler " & Integer'Image(Traveler_Manager_Index) & " arrived without traveling!",
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


					-- # Now let's check whether the travel is finished or not.
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
							Central_Controller_Interface.Set_Traveler_Status(
								Traveler	=> Traveler_Manager_Index,
								Train		=> Trains.Trains(Train_Descriptor_Index).Id,
								Station		=> Environment.Stations(Environment.Travelers(Traveler_Manager_Index).The_Ticket.Stages(Next_Stage).Next_Station).Get_Name,
								Platform	=> This.ID,
								Action 		=> Central_Controller_Interface.FINISHED);

							-- # Time to come back, switch start and destination stations!
							declare
								To_Switch	: Unbounded_String := Environment.Travelers(Traveler_Manager_Index).Start_Station;
							begin
								Environment.Travelers(Traveler_Manager_Index).Start_Station := Environment.Travelers(Traveler_Manager_Index).Destination;
								Environment.Travelers(Traveler_Manager_Index).Destination := To_Switch;
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
						-- # Set the new Operation Index
						Environment.Travelers(Traveler_Manager_Index).Next_Operation := Next_Operation;

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

		if Action = Route.ENTER then
			-- #
			-- # Boarding of Travelers
			-- #
			Logger.Log(
					Sender  => NAME,
					Message => "Train " & Integer'Image(Trains.Trains(Train_Descriptor_Index).Id) & " Performs Boarding of travelers",
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
							Environment.Travelers(Traveler_Manager_Index).Start_Station := This.S.all;
							Traveler_Pool.Execute(Environment.Operations(Traveler_Manager_Index)(Traveler.BUY_TICKET));
						else
							-- # If the current Traveler was not waiting for this train, re-queue it
							This.Leaving_Queue.Enqueue(Traveler_Manager_Index);
						end if;
						null;
					end;
				else

					if 	Environment.Travelers(Traveler_Manager_Index).The_Ticket.Stages(Next_Stage).Current_Run_Id > 0 and
						This.Get_Run_For_Train(Trains.Trains(Train_Descriptor_Index).Id) >
						Environment.Travelers(Traveler_Manager_Index).The_Ticket.Stages(Next_Stage).Current_Run_Id then

						Put_Line("" & integer'image(This.Get_Run_For_Train(Trains.Trains(Train_Descriptor_Index).Id)));
						Put_Line("" & integer'image(Environment.Travelers(Traveler_Manager_Index).The_Ticket.Stages(Next_Stage).Current_Run_Id));

						Logger.log(
							Sender 	=> "Platform",
							Message => "Traveler " & Integer'Image(Traveler_Manager_Index) & " lost the booked Train; buy a new Ticket!",
							L		=> Logger.ERROR);

						Environment.Travelers(Traveler_Manager_Index).Start_Station := This.S.all;
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
							--Traveler_Pool.Execute(Environment.Operations(Traveler_Manager_Index)(Next_Operation));
							-- # Set the new Operation Index
							Environment.Travelers(Traveler_Manager_Index).Next_Operation := Next_Operation;

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
		end if;
    end Perform_Exit;


	procedure Enter(
		This 					: access Platform_Handler;
		Train_Descriptor_Index 	: in 	Positive;
		Action 					: in	Route.Action) is
	begin
		if Trains.Trains(Train_Descriptor_Index).T_Type = Train.FB then
			This.The_Platform.Enter_FB(
				Train_Descriptor_Index 	=> Train_Descriptor_Index);
		else
			This.The_Platform.Enter_Regional(
				Train_Descriptor_Index 	=> Train_Descriptor_Index);
		end if;

		-- # At this point the Task Train has access to the platform!
		Central_Controller_Interface.Set_Train_Status(
			Train		=> Trains.Trains(Train_Descriptor_Index).ID,
			Station		=> To_String(This.S.all),
			Platform	=> Routes.All_Routes(Trains.Trains(Train_Descriptor_Index).Route_Index)
											(Trains.Trains(Train_Descriptor_Index).Next_Stage).Platform_Index,
			Time		=> 1,
			Segment		=> 1,
			Action		=> Central_Controller_Interface.ENTER);

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

		-- # Lets the Train leave the platform
		This.Perform_Exit(
			Train_Descriptor_Index	=> Train_Descriptor_Index,
			Action					=> Action);

		-- # Then set to Free the protected resource
		This.The_Platform.Leave(
			Train_Descriptor_Index 	=> Train_Descriptor_Index);
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
