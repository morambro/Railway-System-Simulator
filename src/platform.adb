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

package body Platform is

	NAME : constant String := "Platfrom.Platform_Type";

	protected body Platform_Type is

		-- #
		-- # Entry At which trains are re-queued to enter the station. Once a train enters the Platform,
		-- # it performs alighting and boarding of the Travelers at the platform.
		-- #
		entry Enter(Train_D : Train_Descriptor) when Free = True is
			Arrival_Number 	: Count_Type := Arrival_Queue.Current_Use;
			Leaving_Number 	: Count_Type := Leaving_Queue.Current_Use;
			T_Manager		: Positive;
			Next_Stage 		: Positive;
		begin
			Free := False;
			-- #
			-- # Alighting of Travelers
			-- #
			Logger.Log(
					Sender  => NAME,
					Message => "Train " & Integer'Image(Train_D.Id) & " Performs Alighting of travelers",
					L       => Logger.NOTICE);
			for I in 1..Arrival_Number loop
				Arrival_Queue.Dequeue(T_Manager);
				Next_Stage := Environment.Get_Travelers(T_Manager).Ticket.Next_Stage;
				if Environment.Get_Travelers(T_Manager).Ticket.Stages(Next_Stage).Train_ID /= Train_D.Id then
					-- # If the current Traveler was not waiting for this train, re-queue it
					Arrival_Queue.Enqueue(T_Manager);
				else
					Logger.Log(
							Sender  => NAME,
							Message => "Passenger " &
										Ada.Strings.Unbounded.To_String(Environment.Get_Travelers(T_Manager).Traveler.Name) &
									   " Leaves the train at station " &
									   Integer'Image(Environment.Get_Travelers(T_Manager).Ticket.Stages(Next_Stage).Next_Station),
							L       => Logger.NOTICE);

					-- # Check if there are more stages, otherwise STOP
					if 	Environment.Get_Travelers(T_Manager).Ticket.Next_Stage =
						Environment.Get_Travelers(T_Manager).Ticket.Stages'Length then
						Logger.Log(
							Sender  => NAME,
							Message => "Passenger " &
										Ada.Strings.Unbounded.To_String(Environment.Get_Travelers(T_Manager).Traveler.Name) &
									   " FINISHED HIS TRAVEL" &
									   Integer'Image(Environment.Get_Travelers(T_Manager).Ticket.Stages(Next_Stage).Next_Station),
							L       => Logger.NOTICE);
					else
						declare
							Next_Operation : Positive := 1;
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

			-- #
			-- # Boarding of Travelers
			-- #
			Logger.Log(
					Sender  => NAME,
					Message => "Train " & Integer'Image(Train_D.Id) & " Performs Boarding of travelers",
					L       => Logger.NOTICE);
			for I in 1..Leaving_Number loop

				-- # Retrieve the next Traveler Manager Index
				Leaving_Queue.Dequeue(T_Manager);
				-- # Retrieve the Next_Stage Index
				Next_Stage := Environment.Get_Travelers(T_Manager).Ticket.Next_Stage;

				if Environment.Get_Travelers(T_Manager).Ticket.Stages(Next_Stage).Train_ID /= Train_D.Id then
					-- # If the current Traveler was not waiting for this train, re-queue it
					Leaving_Queue.Enqueue(T_Manager);
				else
					-- # If the current traveler have to board to the train...
					Logger.Log(
						Sender  => NAME,
						Message => "Passenger " &
								   Ada.Strings.Unbounded.To_String(Environment.Get_Travelers(T_Manager).Traveler.Name) &
								   " boarding at station " &
								   Integer'Image(Environment.Get_Travelers(T_Manager).Ticket.Stages(Next_Stage).Next_Station),
						L       => Logger.NOTICE);


					declare
						-- # The next Traveler operation
						Next_Operation : Positive := 2;
					begin
						-- # Go to the next stage (there will be at least another one for sure!)
						Environment.Get_Travelers(T_Manager).Ticket.Next_Stage :=
							Environment.Get_Travelers(T_Manager).Ticket.Next_Stage +1;
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

		end Enter;

		procedure Leave(Descriptor : in out Train_Descriptor) is
		begin
			Free := True;
		end Leave;


		procedure Add_Incoming_Traveler(Traveler : Positive) is
		begin
			Arrival_Queue.Enqueue(Traveler);
		end Add_Incoming_Traveler;


		procedure Add_Outgoing_Traveler(Traveler : Positive) is
		begin
			Leaving_Queue.Enqueue(Traveler);
			Logger.Log(
				NAME,
				"Travelers in queue = " & Count_Type'Image(Arrival_Queue.Current_Use),
				Logger.DEBUG);

		end Add_Outgoing_Traveler;

	end Platform_Type;

end Platform;
