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

with Ada.Text_IO;use Ada.Text_IO;
with Ada.Characters.Latin_1;use Ada.Characters.Latin_1;
with Logger;
with Environment;
with Trains;
with Routes;
with Route;

package body Segment is

	NAME : constant String := "Segment.Segment_Type";

	protected body Segment_Type is

		-- #
		-- # First entry to implement the protocol. If the Segment is Free, the train executing inside the protected
		-- # resource sets Free := false, then adds himself to running trains queue. If the Segment is not Free, if
		-- # the direction is the same as the direction of the already running trains, it can be added to the queue.
		-- # If the Segment is not free and the direction is different from the other train's, the train is requeued
		-- # to another entry to wait until the Segment becomes free.
		-- #
		entry Enter(
			To_Add 		: in	 Positive;
			Max_Speed 	: 	 out Positive;
			Leg_Length 	:	 out Positive) when True is
		begin

			-- # If the Train tries to access from a third Station, raise an exception
			if 	(Trains.Trains(To_Add).Current_Station /= First_End) and (Trains.Trains(To_Add).Current_Station /= Second_End) then

				raise Bad_Segment_Access_Request_Exception with
					"Invalid Train's origin station, found " & Integer'Image(Trains.Trains(To_Add).Current_Station) &
					" Instead of " & Integer'Image(First_End) & " or " & Integer'Image(Second_End);
			end if;

			-- # The case in which the Segment is Free, so no other Train is Running
			if Free then
				-- # If the Maximum Number of Trains per side was reached for the current direction...
				if Train_Entered_Per_Direction = Max_Trains then
					-- # If opposite queue is not empty, wait
					if Trains.Trains(To_Add).Current_Station = First_End then
						if Second_End_In_Queue > 0 then
							-- REQUEUE TO WAIT
							First_End_In_Queue := First_End_In_Queue + 1;
							requeue Wait;
						end if;
					else
						if First_End_In_Queue > 0 then
							-- REQUEUE TO WAIT
							Second_End_In_Queue := Second_End_In_Queue + 1;
							requeue Wait;
						end if;
					end if;
				end if;
				-- In any other cases, the Train can ENTER
				Free := False;
				Current_Direction := Trains.Trains(To_Add).Current_Station;

				if Current_Direction = Trains.Trains(To_Add).Current_Station then
					-- #  Increase the number of entered trains for this direction
					Train_Entered_Per_Direction := Train_Entered_Per_Direction + 1;
				else
					-- # New Direction, the limit is set to 1
					Train_Entered_Per_Direction := 1;
				end if;

			else
				-- # Segment not FREE
				if Current_Direction = Trains.Trains(To_Add).Current_Station then
					if Train_Entered_Per_Direction = Max_Trains then
						if Trains.Trains(To_Add).Current_Station = First_End then
							if Second_End_In_Queue > 0 then
								-- REQUEUE TO WAIT
								First_End_In_Queue := First_End_In_Queue + 1;
								requeue Wait;
							end if;
						else
							if First_End_In_Queue > 0 then
								-- REQUEUE TO WAIT
								Second_End_In_Queue := Second_End_In_Queue + 1;
								requeue Wait;
							end if;
						end if;
					end if;
				else
					-- REQUEUE TO WAIT in both this cases
					if Trains.Trains(To_Add).Current_Station = First_End then
						First_End_In_Queue := First_End_In_Queue + 1;
						requeue Wait;
					else
						Second_End_In_Queue := Second_End_In_Queue + 1;
						requeue Wait;
					end if;
				end if;
				-- In any other cases, the Train can ENTER

				-- # Only if the Number of Trains per side is not the maximum, increment it.
				if Train_Entered_Per_Direction < Max_Trains then
					Train_Entered_Per_Direction := Train_Entered_Per_Direction + 1;
				end if;
			end if;

			-- Here, the current train gained access to the Segment, so performs
			-- parameters settings.
			-- Add the train to the train queue
			Trains_Number 		:= Trains_Number + 1;

			Running_Trains.Enqueue(Trains.Trains(To_Add).ID);

			if Trains.Trains(To_Add).Max_Speed < Current_Max_Speed then
				Current_Max_Speed := Trains.Trains(To_Add).Max_Speed;
			end if;

			-- Set parameters
			if Segment_Max_Speed > Current_Max_Speed then
				Max_Speed := Current_Max_Speed;
			else
				Max_Speed := Segment_Max_Speed;
			end if;
			Leg_Length := Segment_Length;

			Logger.Log(
				NAME,
				"Train " & Integer'Image(Trains.Trains(To_Add).ID) & " added to running trains queue",
				Logger.DEBUG);

		end Enter;

		-- #
		-- # In this entry the guard Can_Retry_Enter is opened (true) if and only if all running trains leaved
		-- # the Segment, and there are trains queued. If a train task executed inside the entry body, the Segment will be
		-- # still free, but due to Ada protected resource model only trains queued by this entry will run!
		-- # And thanks to the access protocol, here there will be only trains queued coming from the
		-- # same station.
		-- #
		entry Wait(
			To_Add 		: 	in 	 	Positive;
			Max_Speed 	: 		out Positive;
			Leg_Length 	: 		out Positive) when Can_Retry_Enter is
		begin

			if Enter_Retry_Num <= 1 then
				Can_Retry_Enter := False;
			end if;

			Enter_Retry_Num := Enter_Retry_Num - 1;

			-- # We are for sure in the case in which the Segment is Free!
			-- # If the Maximum Number of Trains per side was reached for the current direction...
			if Train_Entered_Per_Direction = Max_Trains then
				-- # If opposite queue is not empty, wait
				if Trains.Trains(To_Add).Current_Station = First_End then
					if Second_End_In_Queue > 0 then
						-- REQUEUE TO WAIT
						First_End_In_Queue := First_End_In_Queue + 1;
						requeue Wait;
					end if;
				else
					if First_End_In_Queue > 0 then
						-- REQUEUE TO WAIT
						Second_End_In_Queue := Second_End_In_Queue + 1;
						requeue Wait;
					end if;
				end if;
			end if;

			-- # In any other cases, the Train can ENTER

			-- # Decrease the number of Trains waiting per side
			if Trains.Trains(To_Add).Current_Station = First_End then
				First_End_In_Queue := First_End_In_Queue - 1;
			else
				Second_End_In_Queue := Second_End_In_Queue - 1;
			end if;


			Free := False;
			Current_Direction := Trains.Trains(To_Add).Current_Station;

			-- # If the Train can enter but the direction is not the same as
			-- # before, set the number of Trains per direction to 0.
			if Current_Direction /= Trains.Trains(To_Add).Current_Station then
				Train_Entered_Per_Direction := 0;
			end if;

			Train_Entered_Per_Direction := Train_Entered_Per_Direction + 1;

			-- # Add the train to the train queue
			Trains_Number := Trains_Number + 1;
			Running_Trains.Enqueue(Trains.Trains(To_Add).ID);

			-- Set parameters
			if Segment_Max_Speed > Current_Max_Speed then
				Max_Speed := Current_Max_Speed;
			else
				Max_Speed := Segment_Max_Speed;
			end if;

			Leg_Length := Segment_Length;

			Logger.Log(
				NAME,
				"Train " & Integer'Image(Trains.Trains(To_Add).ID) & " added to running trains queue",
				Logger.DEBUG);

		end Wait;

-- ############################################### LEAVE ENTRIES #############################################
		-- #
		-- # Entry called by the Train to leave the Segment and access the Next Station.
		-- #
		entry Leave(Train_D : in Positive) when not Free is
		begin
			-- # If the current Train is supposed to be the next to enter
			if(Running_Trains.Get(1) = Trains.Trains(Train_D).ID) then

				-- # Remove the Train ID from the queue
				declare
					T : Positive;
				begin
					Running_Trains.Dequeue(T);
				end;

				-- # If there is at least one train in exit queue, open the guard
				-- # to let it leave the segment
				if(Retry'Count > 0) then
					Retry_Num := Retry'Count;
					Can_Retry_Leave := True;
				end if;


				Logger.Log(
					NAME,
					"Train " & Integer'Image(Trains.Trains(Train_D).ID) & " Leaves!",
					Logger.DEBUG);

				-- Decrease the number of running trains by one
				Trains_Number := Trains_Number - 1;

				-- # If there is no other train running
				if( Trains_Number = 0 ) then
					-- # If there is at least one train waiting to enter the Segment,
					-- # Open the guard!
					if( Wait'Count > 0) then
						Enter_Retry_Num := Wait'Count;
						Can_Retry_Enter := True;
					else
						Free := True;
					end if;
				end if;

				-- # Add the current Train ID to the next station's Access control queue
 				if Current_Direction /= First_End then
					Environment.Stations(First_End).Add_Train(
						Train_ID 	=> Train_D,
						Segment_ID	=> Id
					);
				else
					Environment.Stations(Second_End).Add_Train(
						Train_ID 	=> Train_D,
						Segment_ID	=> Id
					);
				end if;

			else
				Logger.Log(
					NAME,
					"Train " & Integer'Image(Trains.Trains(Train_D).ID) & " Can not leave because it's not the first",
					Logger.DEBUG);
				requeue Retry;
			end if;

		end Leave;


		-- #
		-- # Entry used to perform ordered exit from the Segment
		-- #
		entry Retry(Train_D : in Positive) when Can_Retry_Leave is
		begin

			-- # Decrease the number of tasks that can retry to exit
			Retry_Num := Retry_Num - 1;
			-- # If the number of retry-tasks is 0, close the guard
			if(Retry_Num = 0) then
				Can_Retry_Leave := False;
			end if;

			-- # If the current task (Train) can enter...
			if(Running_Trains.Get(1) = Trains.Trains(Train_D).ID) then

				-- # Remove the Train ID from the queue
				declare
					T : Positive;
				begin
					Running_Trains.Dequeue(T);
				end;

				-- # If there are tasks trying to exit, open the guard.
				if(Retry'Count > 0) then
					Retry_Num := Retry'Count;
					Can_Retry_Leave := True;
				end if;

				Logger.Log(
					NAME,
					"Train " & Integer'Image(Trains.Trains(Train_D).ID) & " Leaves!",
					Logger.DEBUG);

				-- # Decrease the number of Trains in the Segment.
				Trains_Number := Trains_Number - 1;

				-- Set to Free the Segment if no other train is in or waiting
				if( Trains_Number = 0 ) then
					if( Wait'Count > 0) then
						Enter_Retry_Num := Wait'Count;
						Can_Retry_Enter := True;
					else
						Free := True;
						Current_Direction := 0;
					end if;
				end if;

				-- # Add the current Train ID to the next station's Access control queue
 				if Current_Direction /= First_End then
					Environment.Stations(Second_End).Add_Train(
						Train_ID 	=> Train_D,
						Segment_ID	=> Id
					);
				else
					Environment.Stations(Second_End).Add_Train(
						Train_ID 	=> Train_D,
						Segment_ID	=> Id
					);
				end if;

			else
				Logger.Log(
					NAME,
					"Train " & Integer'Image(Trains.Trains(Train_D).ID) & " Can not leave because is not the first",
					Logger.DEBUG);
				requeue Retry;
			end if;
		end Retry;

	end Segment_Type;

-- ########################################### JSON -> Segment FUNCTIONS ###########################################

	function Get_Segment(Json_Segment : Json_Value) return access Segment_Type
	is
		-- Retrieve all the needed fields
		Segment_Id 		: Natural	:= Json_Segment.Get("id");
		Max_Speed 		: Natural	:= Json_Segment.Get("max_speed");
		Queue_Dim		: Natural	:= Json_Segment.Get("queue_dim");
		Segment_Length	: Positive 	:= Json_Segment.Get("length");
		First_End 		: Positive 	:= Json_Segment.Get("first_end");
		Second_End 		: Positive 	:= Json_Segment.Get("second_end");
		Max_Trains 		: Positive 	:= Json_Segment.Get("max_trains");
		-- instantiate the new Segment
		New_Segment 		: access Segment_Type := new Segment_Type(
			Id => Segment_Id,
			Segment_Max_Speed => Max_Speed,
			Segment_Length => Segment_Length,
			Queue_Dim => Queue_Dim,
			First_End => First_End,
			Second_End => Second_End,
			Max_Trains => Max_Trains
		);
	begin
		if
			not Json_Segment.Has_Field(Field => "id") or
			not Json_Segment.Has_Field(Field => "max_speed") or
			not Json_Segment.Has_Field(Field => "queue_dim") or
			not Json_Segment.Has_Field(Field => "length")
		then
			return null;
		end if;
		return New_Segment;
	end Get_Segment;

	function Get_Segment_Array(Json_v : Json_Value) return access Segments_Array is
		-- Extract "Segments" json array in J_Array variable
		J_Array : JSON_Array := Json_v.Get(Field => "segments");
		-- Extract J_Array length
		Array_Length : constant Natural := Length (J_Array);
		-- Instantiate a new Segments_Array with Array_Length elements
		T : access Segments_Array := new Segments_Array(1 .. Array_Length);

	begin
		-- For each element of the json array, create a new Segment
		for I in 1 .. T'Length loop
			T(I) := Get_Segment(Get(Arr => J_Array, Index => I));
		end loop;

		return T;
	end Get_Segment_Array;

	function Get_Segment_Array(File_Name : String) return access Segments_Array is
	begin
		return Get_Segment_Array(Get_Json_Value(Json_File_Name => File_Name));
    end Get_Segment_Array;


    procedure Print(Segment : access Segment_Type) is
    begin
		Put_Line(
			CR & LF &
			"Segment ID : " & Integer'Image(Segment.Id) & CR & LF &
			"Max Speed : " & Integer'Image(Segment.Id) & CR & LF &
			"Segment Length : " & Integer'Image(Segment.Segment_Length) & CR & LF &
			"First End : Station " & Integer'Image(Segment.First_End) & CR & LF &
			"Second End : Station " & Integer'Image(Segment.Second_End));
    end Print;

end Segment;
