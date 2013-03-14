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

package body Platform is

	NAME : constant String := "Platfrom.Platform_Type";

	protected body Platform_Type is

		-- #
		-- # Entry At which trains are re-queued to enter the station. Once a train enters the Platform,
		-- # it performs alighting and boarding of the Travelers at the platform.
		-- #
		entry Enter(T : Train_Descriptor) when Free = True is
			Arrival_Number 	: Count_Type := Arrival_Queue.Current_Use;
			Leaving_Number 	: Count_Type := Leaving_Queue.Current_Use;
			T_Manager		: Traveler_Manager;
		begin
			Free := False;

			-- #
			-- # Alighting of Travelers
			-- #
			Logger.Log(
					Sender  => NAME,
					Message => "Train " & Integer'Image(T.Id) & " Performs Alighting of travelers",
					L       => Logger.NOTICE);
			for I in 1..Arrival_Number loop
				Arrival_Queue.Dequeue(T_Manager);
				if T_Manager.Ticket.Stages(T_Manager.Ticket.Next_Stage).Train_ID /= T.Id then
					-- # If the current Travelere was not waiting for this train, requeue it
					Arrival_Queue.Enqueue(T_Manager);
				else
					Logger.Log(
						Sender  => NAME,
						Message => "Passenger " & Ada.Strings.Unbounded.To_String(T_Manager.Traveler.Name) &
								   " Leaves the train",
						L       => Logger.NOTICE);
				end if;
			end loop;

			-- #
			-- # Boarding of Travelers
			-- #
			Logger.Log(
					Sender  => NAME,
					Message => "Train " & Integer'Image(T.Id) & " Performs Boarding of travelers",
					L       => Logger.NOTICE);
			for I in 1..Leaving_Number loop
				Leaving_Queue.Dequeue(T_Manager);
				if T_Manager.Ticket.Stages(T_Manager.Ticket.Next_Stage).Train_ID /= T.Id then
					-- # If the current Travelere was not waiting for this train, requeue it
					Leaving_Queue.Enqueue(T_Manager);
				else
					Logger.Log(
						Sender  => NAME,
						Message => "Passenger " & Ada.Strings.Unbounded.To_String(T_Manager.Traveler.Name) &
								   " boarding",
						L       => Logger.NOTICE);
				end if;
			end loop;

		end Enter;

		procedure Leave(Descriptor : in out Train_Descriptor) is
		begin
			Free := True;
		end Leave;


		procedure Add_Incoming_Traveler(Traveler : access Traveler_Manager) is
		begin
			Arrival_Queue.Enqueue(Traveler.all);
		end Add_Incoming_Traveler;


		procedure Add_Outgoing_Traveler(Traveler : access Traveler_Manager) is
		begin
			Leaving_Queue.Enqueue(Traveler.all);
			Logger.Log(
				NAME,
				"Travelers in queue = " & Count_Type'Image(Arrival_Queue.Current_Use),
				Logger.DEBUG);

		end Add_Outgoing_Traveler;

	end Platform_Type;

end Platform;
