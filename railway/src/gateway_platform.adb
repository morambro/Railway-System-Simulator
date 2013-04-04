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
with Routes;

package body Gateway_Platform is

	NAME : constant String := "Platfrom.Platform_Type";

	protected body Gateway_Platform_Type is

		-- #
		-- # Entry At which trains are re-queued to enter the station. Once a train enters the Platform,
		-- # it performs alighting and boarding of the Travelers at the platform.
		-- #
		entry Enter(Train_D : in Positive) when Free is
			Arrival_Number 	: Count_Type := Arrival_Queue.Current_Use;
			T_Manager		: Positive;
			Next_Stage 		: Positive;
		begin
			Free := False;
			-- #
			-- # Alighting of Travelers
			-- #
			Logger.Log(
					Sender  => NAME,
					Message => "Train " & Integer'Image(Trains.Trains(Train_D).Id) & " Performs Alighting of travelers" &
								" plat " & Integer'Image(ID),
					L       => Logger.NOTICE);

			for I in 1..Arrival_Number loop
				-- # in T_Manager there will be the index of the next Traveler
				Arrival_Queue.Dequeue(T_Manager);

				-- # Retrieve the next stage from the ticket of the current Traveler
				Next_Stage := Environment.Get_Travelers(T_Manager).Ticket.Next_Stage;
				if Environment.Get_Travelers(T_Manager).Ticket.Stages(Next_Stage).Train_ID /= Trains.Trains(Train_D).Id then
					-- # If the current Traveler was not waiting for this train, re-queue it
					Arrival_Queue.Enqueue(T_Manager);
				else

					-- # Decrease the number of occupied sits
					if Trains.Trains(Train_D).Occupied_Sits > 0 then
						Trains.Trains(Train_D).Occupied_Sits := Trains.Trains(Train_D).Occupied_Sits - 1;
					else
						Logger.Log(
							Sender 	=> NAME,
							Message => "ERROR : Traveler " & Integer'Image(T_Manager) & " arrived without traveling!",
							L		=> Logger.ERROR
						);
					end if;

					Logger.Log(
							Sender  => NAME,
							Message => "Traveler " &
										Integer'Image(Environment.Get_Travelers(T_Manager).Traveler.ID) &
									   " Leaves the train at station " &
									   Integer'Image(Environment.Get_Travelers(T_Manager).Ticket.Stages(Next_Stage).Next_Station),
							L       => Logger.NOTICE);

					-- # Check if there are more stages, otherwise STOP
					if 	Environment.Get_Travelers(T_Manager).Ticket.Next_Stage =
						Environment.Get_Travelers(T_Manager).Ticket.Stages'Length then
						Logger.Log(
							Sender  => NAME,
							Message => "Traveler " &
										Integer'Image(Environment.Get_Travelers(T_Manager).Traveler.ID) &
									   " FINISHED HIS TRAVEL" &
									   Integer'Image(Environment.Get_Travelers(T_Manager).Ticket.Stages(Next_Stage).Next_Station),
							L       => Logger.NOTICE);
					else

						-- # Go to the next stage
						Environment.Get_Travelers(T_Manager).Ticket.Next_Stage :=
							Environment.Get_Travelers(T_Manager).Ticket.Next_Stage + 1;


						declare
							Next_Operation : Traveler.Move_Operations := Traveler.LEAVE;
						begin
							-- # Execute the operation number 2 (Traveler waits to leave the train).
							Task_Pool.Execute(Environment.Get_Operations(T_Manager)(Next_Operation));
							-- # Set the new Operation Index
							Environment.Get_Travelers(T_Manager).Next_Operation := Next_Operation;

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
		end Enter;



		procedure Add_Outgoing_Traveler(Traveler : Positive) is

			Next_Stage_Region : Unbounded_String :=
								Environment.Get_Travelers(Traveler).Ticket.Stages(Environment.Get_Travelers(Traveler).Ticket.Next_Stage).Region;

		begin

			if Next_Stage_Region /= Environment.Get_Node_Name then
				-- #
				-- #
				-- #
				-- # TODO : Transfer to the corresponding Gateway station (to "the other side")
				-- #
				-- #
				-- #
				null;
			else

				Leaving_Queue.Enqueue(Traveler);
				Logger.Log(
					NAME,
					"Travelers in queue = " & Count_Type'Image(Arrival_Queue.Current_Use),
					Logger.DEBUG);
			end if;

		end Add_Outgoing_Traveler;




		procedure Add_Incoming_Traveler(Traveler : Positive) is
		begin
			Arrival_Queue.Enqueue(Traveler);
		end Add_Incoming_Traveler;



		-- #
		-- #
		-- #
		procedure Leave(Train_D : in Positive) is
			Leaving_Number 	: Count_Type := Leaving_Queue.Current_Use;
			T_Manager		: Positive;
			Next_Stage 		: Positive;
		begin
			Free := True;

			-- #
			-- # Boarding of Travelers
			-- #
			Logger.Log(
					Sender  => NAME,
					Message => "Train " & Integer'Image(Trains.Trains(Train_D).Id) & " Performs Boarding of travelers",
					L       => Logger.NOTICE);
			for I in 1..Leaving_Number loop

				-- # Retrieve the next Traveler Manager Index
				Leaving_Queue.Dequeue(T_Manager);


				-- # Retrieve the Next_Stage Index
				Next_Stage := Environment.Get_Travelers(T_Manager).Ticket.Next_Stage;

				if Environment.Get_Travelers(T_Manager).Ticket.Stages(Next_Stage).Train_ID /= Trains.Trains(Train_D).Id then
					-- # If the current Traveler was not waiting for this train, re-queue it
					Leaving_Queue.Enqueue(T_Manager);
				else
					-- # Increase the number of occupied sits
					Trains.Trains(Train_D).Occupied_Sits := Trains.Trains(Train_D).Occupied_Sits + 1;

					-- # If the current traveler have to board to the train...
					Logger.Log(
						Sender  => NAME,
						Message => "Traveler " &
								   Integer'Image(Environment.Get_Travelers(T_Manager).Traveler.ID) &
								   " boarding at station " &
								   Integer'Image(Environment.Get_Travelers(T_Manager).Ticket.Stages(Next_Stage).Next_Station),
						L       => Logger.NOTICE);


					declare
						-- # The next Traveler operation
						Next_Operation : Traveler.Move_Operations := Traveler.ENTER;
					begin
						-- # Execute the operation number 2 (Traveler waits to leave the train).
						Task_Pool.Execute(Environment.Get_Operations(T_Manager)(Next_Operation));
						-- # Set the new Operation Index
						Environment.Get_Travelers(T_Manager).Next_Operation := Next_Operation;

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
						   Integer'Image(Trains.Trains(Train_D).Id) &
						   " has" & Integer'Image(Trains.Trains(Train_D).Occupied_Sits) & "/" &
						   Integer'Image(Trains.Trains(Train_D).Sits_Number) & " travelers",
				L       => Logger.NOTICE);

		end Leave;



	end Gateway_Platform_Type;

end Gateway_Platform;
