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

	NAME : constant String := "Gateway_Platform.Gateway_Platform_Type";

	protected body Gateway_Platform_Type is

		-- #
		-- # Entry At which trains are re-queued to enter the station. Once a train enters the Platform,
		-- # it performs alighting and boarding of the Travelers at the platform.
		-- #
		entry Enter(
			Train_Descriptor_Index 	: in 	Positive;
			Action 					: in	Route.Action)
		when Free is
			Arrival_Number 	: Count_Type := Arrival_Queue.Current_Use;
			Traveler_Manager_Index		: Positive;
			Next_Stage 		: Positive;
		begin

			Free 			:= False;
			Current_Train 	:= Train_Descriptor_Index;

			-- #
			-- # Alighting of Travelers
			-- #
			case Action is
				when Route.ENTER =>

					Logger.Log(
						Sender  => NAME,
						Message => "Train " & Integer'Image(Trains.Trains(Train_Descriptor_Index).Id) & " Performs Alighting of travelers" &
									" plat " & Integer'Image(ID),
						L       => Logger.DEBUG);


					for I in 1..Arrival_Number loop
						-- # in Traveler_Manager_Index there will be the index of the next Traveler
						Arrival_Queue.Dequeue(Traveler_Manager_Index);

						-- # Retrieve the next stage from the ticket of the current Traveler
						Next_Stage := Environment.Travelers(Traveler_Manager_Index).Ticket.Next_Stage;
						if Environment.Travelers(Traveler_Manager_Index).Ticket.Stages(Next_Stage).Train_ID /= Trains.Trains(Train_Descriptor_Index).Id then
							-- # If the current Traveler was not waiting for this train, re-queue it
							Arrival_Queue.Enqueue(Traveler_Manager_Index);
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

								-- # Go to the next stage
								Environment.Travelers(Traveler_Manager_Index).Ticket.Next_Stage :=
									Environment.Travelers(Traveler_Manager_Index).Ticket.Next_Stage + 1;


								declare
									Next_Operation : Traveler.Move_Operations := Traveler.LEAVE;
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

				when Route.PASS =>
					Logger.Log(
						Sender  => NAME,
						Message => "Train " &
								   Integer'Image(Trains.Trains(Train_Descriptor_Index).Id) &
								   " Passed the platform",
						L       => Logger.DEBUG);

				when others 	=>
					-- # TODO: DEFINE AND RAISE AN EXCEPTION
					null;
			end case;

		end Enter;



		procedure Add_Outgoing_Traveler(Traveler : Positive) is


		begin
			Leaving_Queue.Enqueue(Traveler);
			Logger.Log(
				NAME,
				"Travelers in queue = " & Count_Type'Image(Arrival_Queue.Current_Use),
				Logger.DEBUG);
		end Add_Outgoing_Traveler;




		procedure Add_Incoming_Traveler(Traveler : Positive) is
		begin
			Arrival_Queue.Enqueue(Traveler);
		end Add_Incoming_Traveler;



		-- #
		-- #
		-- #
		procedure Leave(
			Train_Descriptor_Index 	: in 	Positive;
			Action 					: in	Route.Action)
		is
			Leaving_Number 	: Count_Type := Leaving_Queue.Current_Use;
			Traveler_Manager_Index		: Positive;
			Next_Stage 		: Positive;
		begin

			if Current_Train = Train_Descriptor_Index then
				Current_Train 	:= 0;
				Free 			:= True;

				case Action is
					when  Route.ENTER =>
						Put_Line("ACTION = ENTER");

						-- #
						-- # Boarding of Travelers
						-- #
						Logger.Log(
								Sender  => NAME,
								Message => "Train " & Integer'Image(Trains.Trains(Train_Descriptor_Index).Id) & " Performs Boarding of travelers",
								L       => Logger.DEBUG);
						for I in 1..Leaving_Number loop

							-- # Retrieve the next Traveler Manager Index
							Leaving_Queue.Dequeue(Traveler_Manager_Index);


							-- # Retrieve the Next_Stage Index
							Next_Stage := Environment.Travelers(Traveler_Manager_Index).Ticket.Next_Stage;

							if Environment.Travelers(Traveler_Manager_Index).Ticket.Stages(Next_Stage).Train_ID /= Trains.Trains(Train_Descriptor_Index).Id then
								-- # If the current Traveler was not waiting for this train, re-queue it
								Leaving_Queue.Enqueue(Traveler_Manager_Index);
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
									Next_Operation : Traveler.Move_Operations := Traveler.ENTER;
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
						end loop;

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
			end if;

		end Leave;

	end Gateway_Platform_Type;

end Gateway_Platform;
