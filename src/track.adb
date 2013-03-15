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

package body Track is

	NAME : constant String := "Track.Track_Type";

	protected body Track_Type is

		-- #
		-- # First entry to implement the protocol. If the track is Free, the train executing inside the protected
		-- # resource sets Free := false, then adds himself to running trains queue. If the track is not Free, if
		-- # the direction is the same as the direction of the already running trains, it can be added to the queue.
		-- # If the track is not free and the direction is different from the other train's, the train is requeued
		-- # to another entry to wait until the track becomes free.
		-- #
		entry Enter(
			To_Add 		:	in 		Positive;
			Max_Speed 	: 	 	out Positive;
			Leg_Length 	:		out	Positive) when True is
		begin


			if 	(Trains.Trains(To_Add).Current_Station /= First_End) and
				(Trains.Trains(To_Add).Current_Station /= Second_End)
			then
				raise Bad_Track_Access_Request_Exception with "Invalid Train's origin station";
			end if;

			Logger.Log(
				NAME,
				"Train " & Integer'Image(Trains.Trains(To_Add).ID) &
				" current origin : Station " & Integer'Image(Trains.Trains(To_Add).Current_Station),
				Logger.DEBUG);

			-- If the track is Free, set it to not Free, and set also the current "direction"
			if Free then
				Logger.Log(
					NAME,
					"Train " & Integer'Image(Trains.Trains(To_Add).ID) & " Enters, Track was free",
					Logger.DEBUG);
				Current_Direction := Trains.Trains(To_Add).Current_Station;
				Free := False;
			else
				-- If the Track is not Free ad the current train direction is not the same as the direction
				-- of the already running train(s), re-queue to Wait entry.
				if ( Trains.Trains(To_Add).Current_Station /= Current_Direction) then
					-- In case The track is not null, move the task to a waiting queue
					Logger.Log(
						NAME,
						"Train " & Integer'Image(Trains.Trains(To_Add).ID) & " will be re-queued, wrong direction",
						Logger.DEBUG);
					requeue Wait;
				end if;
			end if;

			-- Here, the current train gained access to the track, so performs
			-- parameters settings.
			-- Add the train to the train queue
			Trains_Number := Trains_Number + 1;
			Running_Trains(Trains_Number) := Trains.Trains(To_Add).ID;

			if Trains.Trains(To_Add).Max_Speed < Current_Max_Speed then
				Current_Max_Speed := Trains.Trains(To_Add).Max_Speed;
			end if;

			-- Set parameters
			if Track_Max_Speed > Current_Max_Speed then
				Max_Speed := Current_Max_Speed;
			else
				Max_Speed := Track_Max_Speed;
			end if;
			Leg_Length := Track_Length;

			Logger.Log(
				NAME,
				"Train " & Integer'Image(Trains.Trains(To_Add).ID) & " added to running trains queue",
				Logger.DEBUG);

		end Enter;

		-- #
		-- # In this entry the guard Can_Retry_Enter is opened (true) if and only if all running trains leaved
		-- # the track, and there are trains queued. If a train task executed inside the entry body, the track will be
		-- # still free, but due to Ada protected resource model only trains queued by this entry will run!
		-- # And thanks to the access protocol, here there will be only trains queued coming from the
		-- # same station.
		-- #
		entry Wait(
			To_Add 		: 	in 	 	Positive;
			Max_Speed 	: 		out Positive;
			Leg_Length 	: 		out Positive)
			when Can_Retry_Enter is
		begin

			if Free then
				Logger.Log(
					NAME,
					"Train " & Integer'Image(Trains.Trains(To_Add).ID) & " Enters, Track was free",
					Logger.DEBUG);
				Free := False;
				Current_Direction := Trains.Trains(To_Add).Current_Station;
			end if;
			-- Add the train to the train queue
			Trains_Number := Trains_Number + 1;
			Running_Trains(Trains_Number) := Trains.Trains(To_Add).ID;

			-- Set parameters
			if Track_Max_Speed > Current_Max_Speed then
				Max_Speed := Current_Max_Speed;
			else
				Max_Speed := Track_Max_Speed;
			end if;

			Leg_Length := Track_Length;

			Logger.Log(
				NAME,
				"Train " & Integer'Image(Trains.Trains(To_Add).ID) & " added to running trains queue",
				Logger.DEBUG);

		end Wait;

---------------------------------------------- LEAVE ENTRIES -----------------------------------------
		-- #
		-- # Entry called by the Train to leave the track and access the Next Station.
		-- #
		entry Leave(Train_D : in Positive) when not Free is
		begin
			if(Running_Trains(1) = Trains.Trains(Train_D).ID) then

				-- Shift all the other trains by one
				for I in Integer range 2 .. Max_Trains loop
					Running_Trains(I-1) := Running_Trains(I);
				end loop;

				-- If there is at least one train in queue, open the guard.
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

				-- If there is no other train running
				if( Trains_Number = 0 ) then
					-- If there is at least one train waiting to enter the track,
					-- Open the guard!
					if( Wait'Count > 0) then
						Can_Retry_Enter := True;
					else
						Free := True;
					end if;
				end if;

				-- Now re-queue the train to the proper platform.
 				if Current_Direction /= First_End then
					requeue Environment.Get_Regional_Stations(First_End).Get_Platform(1).Enter;
				else
					requeue Environment.Get_Regional_Stations(Second_End).Get_Platform(1).Enter;
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
		-- # Entry used to perform ordered exit from the Track
		-- #
		entry Retry(Train_D : in Positive) when Can_Retry_Leave is
		begin

			Retry_Num := Retry_Num - 1;
			if(Retry_Num = 0) then
				Can_Retry_Leave := False;
			end if;

			if(Running_Trains(1) = Trains.Trains(Train_D).ID) then
				for I in Integer range 2 .. 10 loop
					Running_Trains(I-1) := Running_Trains(I);
				end loop;
				if(Retry'Count > 0) then
					Retry_Num := Retry'Count;
					Can_Retry_Leave := True;
				end if;

				Logger.Log(
					NAME,
					"Train " & Integer'Image(Trains.Trains(Train_D).ID) & " Leaves!",
					Logger.DEBUG);

				Trains_Number := Trains_Number - 1;

				-- Set to Free the track if no other train is in or waiting
				if( Trains_Number = 0 ) then
					if( Wait'Count > 0) then
						Can_Retry_Enter := True;
					else
						Free := True;
						Current_Direction := 0;
					end if;
				end if;

				-- Now re-queue the train to the proper platform.
 				if Current_Direction /= First_End then
					requeue Environment.Get_Regional_Stations(First_End).Get_Platform(1).Enter;
				else
					requeue Environment.Get_Regional_Stations(Second_End).Get_Platform(1).Enter;
				end if;

			else
				Logger.Log(
					NAME,
					"Train " & Integer'Image(Trains.Trains(Train_D).ID) & " Can not leave because is not the first",
					Logger.DEBUG);
				requeue Retry;
			end if;
		end Retry;

	end Track_Type;

----------------------------------------- JSON -> TRACK FUNCTIONS ---------------------------------

	function Get_Track(Json_Track : Json_Value) return access Track_Type
	is
		-- Retrieve all the needed fields
		Track_Id 		: Natural	:= Json_Track.Get("id");
		Max_Speed 		: Natural	:= Json_Track.Get("max_speed");
		Queue_Dim		: Natural	:= Json_Track.Get("queue_dim");
		Track_Length	: Positive 	:= Json_Track.Get("length");
		First_End 		: Positive 	:= Json_Track.Get("first_end");
		Second_End 		: Positive 	:= Json_Track.Get("second_end");
		Max_Trains 		: Positive 	:= Json_Track.Get("max_trains");
		-- instantiate the new track
		New_Track 		: access Track_Type := new Track_Type(
			Id => Track_Id,
			Track_Max_Speed => Max_Speed,
			Track_Length => Track_Length,
			Queue_Dim => Queue_Dim,
			First_End => First_End,
			Second_End => Second_End,
			Max_Trains => Max_Trains
		);
	begin
		if
			not Json_Track.Has_Field(Field => "id") or
			not Json_Track.Has_Field(Field => "max_speed") or
			not Json_Track.Has_Field(Field => "queue_dim") or
			not Json_Track.Has_Field(Field => "length")
		then
			return null;
		end if;
		return New_Track;
	end Get_Track;

	function Get_Track_Array(Json_v : Json_Value) return access Tracks_Array is
		-- Extract "tracks" json array in J_Array variable
		J_Array : JSON_Array := Json_v.Get(Field => "tracks");
		-- Extract J_Array length
		Array_Length : constant Natural := Length (J_Array);
		-- Instantiate a new Tracks_Array with Array_Length elements
		T : access Tracks_Array := new Tracks_Array(1 .. Array_Length);

	begin
		-- For each element of the json array, create a new Track
		for I in 1 .. T'Length loop
			T(I) := Get_Track(Get(Arr => J_Array, Index => I));
		end loop;

		return T;
	end Get_Track_Array;

	function Get_Track_Array(File_Name : String) return access Tracks_Array is
	begin
		return Get_Track_Array(Get_Json_Value(Json_File_Name => File_Name));
    end Get_Track_Array;


    procedure Print(Track : access Track_Type) is
    begin
		Put_Line(
			CR & LF &
			"Track ID : " & Integer'Image(Track.Id) & CR & LF &
			"Max Speed : " & Integer'Image(Track.Id) & CR & LF &
			"Track Length : " & Integer'Image(Track.Track_Length) & CR & LF &
			"First End : Station " & Integer'Image(Track.First_End) & CR & LF &
			"Second End : Station " & Integer'Image(Track.Second_End));
    end Print;

end Track;
