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

package body Segment_2 is

	NAME : constant String := "Segment.Segment_Type";

	protected body Segment_Type is

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

			-- # Check for Train Direction.
			if Trains.Trains(To_Add).Current_Station = First_End then
				-- # The Train comes from First_End
				if Free then
					-- # If the Segment is Free, Set the number of Trains per direction
					-- # to 1 and set Free to False.
					Put_Line("FIRST_END - FREE");
					Train_Entered_Per_Direction := 1;
					Free := False;
					-- # Set the Current Direction to the value of the current station.
					Current_Direction := Trains.Trains(To_Add).Current_Station;
					-- # Close the Guard for the second end retry entry.
					Can_Enter_Second_End := False;
				else
					-- # If the Segment is not free
					if Trains.Trains(To_Add).Current_Station = Current_Direction then
						-- # Check if the number of trains per direction reached the maximum allowed
						if Train_Entered_Per_Direction = Max then
							-- # If Train_Entered_Per_Direction reached the maximum, the
							-- # Train is re-queued to Enter_First_End if there are at least a Train
							-- # waiting at the other end of the Segment.
							if Retry_Second_End'Count > 0 then
							 	-- # Close the guard for the First End retry entry.
								Can_Enter_First_End := False;
								requeue Retry_First_End;
							end if;
						else
							-- # If the maximum have not been reached, simply add 1 to the number of
							-- # entered trains per direction.
							Train_Entered_Per_Direction := Train_Entered_Per_Direction + 1;
						end if;
					else
						-- # The current Train is going to the opposite direction, so it will wait.
						Can_Enter_First_End := False;
						requeue Retry_First_End;
					end if;
				end if;
			else
				if Free then
					Put_Line("SECOND_END - FREE");
					Train_Entered_Per_Direction := 1;
					Free := False;
					Current_Direction := Trains.Trains(To_Add).Current_Station;
					Can_Enter_First_End := False;
				else
					if Trains.Trains(To_Add).Current_Station = Current_Direction then
						if Train_Entered_Per_Direction = Max then
							if Retry_First_End'Count > 0 then
								Can_Enter_Second_End := False;
								requeue Retry_Second_End;
							end if;
						else
							Train_Entered_Per_Direction := Train_Entered_Per_Direction + 1;
						end if;
					else
						-- # The current Train is going to the opposite direction, so it will wait.
						Can_Enter_Second_End := False;
						requeue Retry_Second_End;
					end if;
				end if;
			end if;

			-- # update the number of running trains
			Trains_Number := Trains_Number + 1;
			-- # add the current Train ID to perform ordered exit.
			Running_Trains.Enqueue(Trains.Trains(To_Add).Id);
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

		entry Retry_First_End(
			To_Add 		:	in 		Positive;
			Max_Speed 	: 	 	out Positive;
			Leg_Length 	:		out	Positive) when Can_Enter_First_End
		is
		begin
			-- # Decrease the number of retries
			Enter_Retry_Num := Enter_Retry_Num - 1;

			-- # If the number is 0, close the guard
			if Enter_Retry_Num = 0 then
				Can_Enter_First_End := False;
			end if;

			requeue Enter;

		end Retry_First_End;

		entry Retry_Second_End(
			To_Add 		:	in 		Positive;
			Max_Speed 	: 	 	out Positive;
			Leg_Length 	:		out	Positive) when Can_Enter_First_End
		is
		begin
			Enter_Retry_Num := Enter_Retry_Num - 1;

			if Enter_Retry_Num = 0 then
				Can_Enter_Second_End := False;
			end if;

			requeue Enter;

		end Retry_Second_End;

-- ############################################### LEAVE ENTRIES #############################################
		-- #
		-- # Entry called by the Train to leave the Segment and access the Next Station.
		-- #
		entry Leave(Train_D : in Positive) when not Free is
		begin
			-- # If the current Train is the next allowed to exit
			if Running_Trains.Get(1) = Trains.Trains(Train_D).ID then

				-- # Remove the Train ID from the queue
				declare
					T : Positive;
				begin
					Running_Trains.Dequeue(T);
				end;

				-- # If there is at least one train in exit queue, open the guard to let it leave the segment
				if(Retry_Leave'Count > 0) then
					Exit_Retry_Num := Retry_Leave'Count;
					Can_Retry_Leave := True;
				end if;

				Logger.Log(
					NAME,
					"Train " & Integer'Image(Trains.Trains(Train_D).ID) & " Leaves!",
					Logger.DEBUG);

				-- Decrease the number of running trains by one
				Trains_Number := Trains_Number - 1;


				if Trains_Number = 0 then
					Current_Max_Speed := Segment_Max_Speed;
					-- # If there are no other Trains running...
					if Current_Direction = First_End then
						-- # If there is at least one train waiting to enter the Segment from the Second
						-- # End, open the guard to let it come in
						if Retry_Second_End'Count > 0 then
							Enter_Retry_Num := Retry_Second_End'Count;
							Can_Enter_Second_End := True;
							Can_Enter_First_End := False;
						else
							-- # If there are no other trains running, set the Segment to Free
							Free := True;
						end if;
					else
						-- # If there is at least one train waiting to enter the Segment from the First
						-- # End, open the guard to let it come in
						if Retry_First_End'Count > 0 then
							Enter_Retry_Num := Retry_First_End'Count;
							Can_Enter_First_End := True;
							Can_Enter_Second_End := False;
						else
							Free := True;
						end if;
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
				requeue Retry_Leave;
			end if;

		end Leave;


		-- #
		-- # Entry used to perform ordered exit from the Segment
		-- #
		entry Retry_Leave(Train_D : in Positive) when Can_Retry_Leave is
		begin
			-- # Decrease the number of tasks that can retry to exit
			Exit_Retry_Num := Exit_Retry_Num - 1;
			-- # If the number of retry-tasks is 0, close the guard
			if(Exit_Retry_Num = 0) then
				Can_Retry_Leave := False;
			end if;
			requeue Leave;
		end Retry_Leave;

	end Segment_Type;

-- ########################################### JSON -> Segment FUNCTIONS ###########################################

	function Get_Segment(Json_Segment : Json_Value) return access Segment_Type
	is
		-- Retrieve all the needed fields
		Segment_Id 		: Natural	:= Json_Segment.Get("id");
		Max_Speed 		: Natural	:= Json_Segment.Get("max_speed");
		Segment_Length	: Positive 	:= Json_Segment.Get("length");
		First_End 		: Positive 	:= Json_Segment.Get("first_end");
		Second_End 		: Positive 	:= Json_Segment.Get("second_end");
		Max				: Natural 	:= Json_Segment.Get("max");
		-- instantiate the new Segment
		New_Segment 		: access Segment_Type := new Segment_Type(
			Id 					=> Segment_Id,
			Segment_Max_Speed 	=> Max_Speed,
			Segment_Length 		=> Segment_Length,
			First_End 			=> First_End,
			Second_End 			=> Second_End,
			Max 				=> Max
		);
	begin
		if
			not Json_Segment.Has_Field(Field => "id") or
			not Json_Segment.Has_Field(Field => "max_speed") or
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

end Segment_2;
