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
--  it under the terms of tIOhe GNU General Public License as published by		--
--  the Free Software Foundation, either version 3 of the License, or			--
--  (at your option) any later version.											--
--																				--
--  Railway_Simulation is distributed in the hope that it will be useful,			--
--  but WITHOUT ANY WARRANTY; without even the implied warranty of				--
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the				--
--  GNU General Public License for more details.								--
--																				--
--  You should have received a copy of the GNU General Public License			--
--  along with Railway_Simulation.  If not, see <http://www.gnu.org/licenses/>.		--
----------------------------------------------------------------------------------
with Trains;
with Logger;
with Ada.Text_IO;use Ada.Text_IO;
with Ada.Containers;use Ada.Containers;

package body Generic_Station is

		protected body Access_Controller is

		entry Enter(
			Train_Index : in 	 Positive) when True is
		begin
			-- # If the currently running Train is the next that can Access the Platform,
			-- # let it pass, otherwise re-queue to Wait entry, until the guard is opened.
			if Trains.Trains(Train_Index).ID /= Trains_Order.Get(1) then
				Logger.Log(
					Sender 	=> "Access_Controller",
					Message	=> "Train " & Integer'Image(Trains.Trains(Train_Index).ID) & " cannot enter, it is not" &
					  				Integer'Image(Trains_Order.Get(1)),
					L 		=> Logger.DEBUG
				);
				requeue Wait;
			end if;

			Logger.Log(
				Sender 	=> "Access_Controller",
				Message => "ENTERED TRAIN " & Integer'Image(Trains.Trains(Train_Index).ID),
				L 		=> Logger.DEBUG);

		end Enter;


		entry Wait(
			Train_Index	: in 	 Positive) when Can_Retry is
		begin
			-- # Decrease the number of re-attempting Trains
			Trains_Waiting := Trains_Waiting - 1;

			-- # If the number is 0, then close the guard.
			if Trains_Waiting = 0 then
				Can_Retry := False;
			end if;

			-- # If the currently running Train is the next that can Access the Platform,
			-- # let it pass, otherwise re-queue to Wait entry, until the guard is opened.
			if Trains.Trains(Train_Index).ID /= Trains_Order.Get(1) then
				requeue Wait;
			end if;
		end Wait;


		procedure Free is
			Train_ID	: Positive;
		begin
			-- # Dequeue the First Train (the one which already called Enter).
			Trains_Order.Dequeue(Train_ID);

			-- # Check if the number of waiting Trains in Wait entry is > 0.
			if Wait'Count > 0 then
				-- # Open the guard to let them retry
				Trains_Waiting := Wait'Count;
				Can_Retry := True;
				Logger.Log(
					Sender 	=> "Access_Controller",
					Message	=> "Train " & Integer'Image(Train_ID) & " opened the guard Wait",
					L 		=> Logger.DEBUG
				);

				if not Trains_Order.Is_Empty then
					Logger.Log(
						Sender 	=> "Access_Controller",
						Message => "NEXT TRAIN TO ENTER IS = " & Integer'Image(Trains_Order.Get(1)),
						L 		=> Logger.DEBUG);
				else
					Logger.Log(
						Sender 	=> "Access_Controller",
						Message => "NO OTHER TRAIN IN QUEUE ",
						L 		=> Logger.DEBUG);
				end if;
			end if;
		end Free;

		procedure Add_Train(
			Train_ID : in 	 Positive) is
		begin
			-- # Add a new element in the Queue.
			Trains_Order.Enqueue(Train_ID);
		end Add_Train;


 	end Access_Controller;

end Generic_Station;
