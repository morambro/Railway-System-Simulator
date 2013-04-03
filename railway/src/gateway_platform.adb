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
with Message_Agent;
with YAMI.Parameters;
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
					Message => "Train " & Integer'Image(Trains.Trains(Train_D).Id) & " Performs Alighting of travelers" &
								" plat " & Integer'Image(ID),
					L       => Logger.NOTICE);

			Put_Line(Count_Type'Image(Arrival_Number));

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
			Put_Line("END");
		end Enter;


		procedure Add_Incoming_Traveler(Traveler : Positive) is
			-- # Based on the next stage, decide weather to transfer the Traveler to another node or not.
			Next_Stage_Region : Unbounded_String :=
				Environment.Get_Travelers(Traveler).Ticket.Stages(Environment.Get_Travelers(Traveler).Ticket.Next_Stage).Region;
		begin
			-- # If the next stage region is different from the current, the Traveler have to be transferred to the corresponding
			-- # gateway station on the specified region
			if  Next_Stage_Region/= Environment.Get_Node_Name then

				-- # SEND to the next Station TODO!!!!
				null;
			else
				Arrival_Queue.Enqueue(Traveler);
			end if;
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
						   Integer'Image(Trains.Trains(Train_D).Sists_Number) & " travelers",
				L       => Logger.NOTICE);

			-- # Send train with descriptor Train_D to the next Station, and stop current Train Execution
			declare
				-- # Retrieve Next Station index
				Next_Station_Index 	: Positive 	:=
					Routes.All_Routes(Trains.Get_Trains(Train_D).Route_Index)(Trains.Get_Trains(Train_D).Next_Stage + 1).Next_Station;
				-- # Retrieve the Node_Name
				Next_Station_Node	: String 	:= To_String(
					Routes.All_Routes(Trains.Get_Trains(Train_D).Route_Index)(Trains.Get_Trains(Train_D).Next_Stage + 1).Node_Name);
			begin
				Send_Train(Train_D,Next_Station_Index,Next_Station_Node);
			end;
		end Leave;


		procedure Add_Outgoing_Traveler(Traveler : Positive) is
		begin
			Leaving_Queue.Enqueue(Traveler);
			Logger.Log(
				NAME,
				"Travelers in queue = " & Count_Type'Image(Arrival_Queue.Current_Use),
				Logger.DEBUG);

		end Add_Outgoing_Traveler;

	end Gateway_Platform_Type;


	procedure Send_Train(
		Train_D 		: in	 Positive;
		Station	 		: in	 Positive;
		Node_Name		: in	 String )
	is
		Parameters : YAMI.Parameters.Parameters_Collection := YAMI.Parameters.Make_Parameters;
	begin

		-- # First, Resolve destination address
		declare

			Dest_Address : Unbounded_String;

			-- # Callback function, used to handle the response
			procedure Get_Results(Content : in out YAMI.Parameters.Parameters_Collection) is

				Address : String := Content.Get_String("address");

			begin
				Logger.Log(
					Sender 	=> "Gateway_Station",
					Message => "The destination is located at address " & Address,
					L 		=> Logger.DEBUG
				);
		    end Get_Results;

		begin

			Parameters.Set_String("station",Integer'Image(Station));
			Parameters.Set_String("node_name",Node_Name);

			Message_Agent.Instance.Send(
				Destination_Address => Environment.Get_Name_Server,
				Object 				=> "name_server",
				Service 			=> "get",
				Params 				=> Parameters,
				Callback			=> Get_Results'Access
			);

			-- # Once the address is resolved, send the Train to the destination node, and interrupt
			-- # the current task Train execution.

			Parameters.Clear;

			Parameters.Set_String("train","{...}");
			Parameters.Set_String("station",Integer'Image(Station));

			Message_Agent.Instance.Send(
				Destination_Address => To_String(Dest_Address),
				Object 				=> "gateway_hadler",
				Service 			=> "train_transfer",
				Params 				=> Parameters,
				Callback			=> null
			);

		exception
			when E : others => Put_Line("Exception");
		end;


		-- # Stop current execution raising an exception.
		raise Gateway_Platform.Stop_Train_Execution;

    end Send_Train;


end Gateway_Platform;
