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
with Ada.Strings.Unbounded;
with Environment;
with Ada.Text_IO;use Ada.Text_IO;
with Task_Pool;
with Ada.Exceptions;
with Trains;
with Traveler;
with Routes;

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
				Next_Stage := Environment.Travelers(Traveler_Manager_Index).Ticket.Next_Stage;
				if Environment.Travelers(Traveler_Manager_Index).Ticket.Stages(Next_Stage).Train_ID /= Trains.Trains(Train_Descriptor_Index).Id then
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
									   Integer'Image(Environment.Travelers(Traveler_Manager_Index).Ticket.Stages(Next_Stage).Next_Station),
							L       => Logger.DEBUG);

					-- # Check if there are more stages, otherwise STOP
					if 	Environment.Travelers(Traveler_Manager_Index).Ticket.Next_Stage =
						Environment.Travelers(Traveler_Manager_Index).Ticket.Stages'Length then
						Logger.Log(
							Sender  => NAME,
							Message => "Traveler " &
										Integer'Image(Environment.Travelers(Traveler_Manager_Index).Traveler.ID) &
									   " FINISHED HIS TRAVEL" &
									   Integer'Image(Environment.Travelers(Traveler_Manager_Index).Ticket.Stages(Next_Stage).Next_Station),
							L       => Logger.DEBUG);
					else

						-- # Go to the next stage (there will be at least another one for sure!)
						Environment.Travelers(Traveler_Manager_Index).Ticket.Next_Stage :=
							Environment.Travelers(Traveler_Manager_Index).Ticket.Next_Stage + 1;

						declare
							Next_Operation : Traveler.Traveler_Operations_Types := Traveler.LEAVE;
						begin
							-- # Execute the operation number 2 (Traveler waits to leave the train).
							Task_Pool.Execute(Environment.Operations(Traveler_Manager_Index)(Next_Operation));
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
				Next_Stage := Environment.Travelers(Traveler_Manager_Index).Ticket.Next_Stage;

				if Environment.Travelers(Traveler_Manager_Index).Ticket.Stages(Next_Stage).Train_ID /= Trains.Trains(Train_Descriptor_Index).Id then
					-- # If the current Traveler was not waiting for this train, re-queue it
					This.Leaving_Queue.Enqueue(Traveler_Manager_Index);
				else
					-- # Increase the number of occupied sits
					Trains.Trains(Train_Descriptor_Index).Occupied_Sits := Trains.Trains(Train_Descriptor_Index).Occupied_Sits + 1;

					-- # If the current traveler have to board to the train...
					Logger.Log(
						Sender  => NAME,
						Message => "Traveler " &
								   Integer'Image(Environment.Travelers(Traveler_Manager_Index).Traveler.ID) &
								   " boarding at station " &
								   Integer'Image(Environment.Travelers(Traveler_Manager_Index).Ticket.Stages(Next_Stage).Next_Station),
						L       => Logger.DEBUG);


					declare
						-- # The next Traveler operation
						Next_Operation : Traveler.Traveler_Operations_Types := Traveler.ENTER;
					begin
						-- # Execute the operation ENTER (Traveler waits to leave the train).
						Environment.Operations(Traveler_Manager_Index)(Next_Operation).Do_Operation;
						--Task_Pool.Execute(Environment.Operations(Traveler_Manager_Index)(Next_Operation));
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

		-- # CODE FOR EXIT...
		This.Perform_Exit(
			Train_Descriptor_Index	=> Train_Descriptor_Index,
			Action					=> Action);


		This.The_Platform.Leave(
			Train_Descriptor_Index 	=> Train_Descriptor_Index);

    end Leave;

	procedure Add_Incoming_Traveler(
		This 					: access Platform_Handler;
		Traveler 				: in 	Positive) is
	begin
		This.Arrival_Queue.Enqueue(Traveler);
    end Add_Incoming_Traveler;

	procedure Add_Outgoing_Traveler(
		This 					: access Platform_Handler;
		Traveler 				: in 	Positive) is
	begin
		This.Leaving_Queue.Enqueue(Traveler);
    end Add_Outgoing_Traveler;



end Platform;
