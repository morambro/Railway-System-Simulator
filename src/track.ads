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

with Train;use Train;
with Gnatcoll.JSON;use Gnatcoll.JSON;
with JSON_Helper;use JSON_Helper;

package Track is

	-- # IDs of currently traveling trains
	type Train_Queue is array (Positive range <>) of Integer;

	Bad_Track_Access_Request_Exception : exception;

	protected type Track_Type(
		Id 				: Natural;
		Track_Max_Speed : Positive;
		-- Length expressed in meters
		Track_Length	: Positive;
		Queue_Dim 		: Positive;
		First_End 		: Positive;
		Second_End 		: Positive;
		Max_Trains		: Positive)

	is

		-- #
		-- # Trains ask to Enter the track
		-- #
		entry Leave(Train_D : in Positive);

		-- #
		-- # Trains ask to Enter the track
		-- #
		entry Enter(
			To_Add 		:	in 		Positive;
			Max_Speed 	: 	 	out Positive;
			Leg_Length 	:		out	Positive);

	private

		-- #
		-- # Private Entry used to enqueue trains whose direction are not the same
		-- # as the direction of already running trains.
		-- #
		entry Wait(
			To_Add 		:	in 		Positive;
			Max_Speed 	: 	 	out Positive;
			Leg_Length 	:		out	Positive);

		-- #
		-- # Private Entry used to enqueue trains, to guarantee an exit order.
		-- #
		entry Retry(Train_D : in Positive);

		-- # Tells weather a train is already running or not
		Free : Boolean := True;

		-- # Current maximum speed
		Current_Max_Speed : Positive := Track_Max_Speed;

		-- # Current direction. Is set to 0 when the track is free
		Current_Direction : Natural := 0;

		-- # Queue of all the running trains
		Running_Trains : Train_Queue (1..Queue_Dim);

		-- # Boolean guard telling if a train can retry to Leave the track
		Can_Retry_Leave : Boolean := False;

		-- # Boolean guard telling if a train can retry to Enter the track
		Can_Retry_Enter : Boolean := False;

		-- # Number of exit attempt
		Retry_Num : Integer := 0;

		-- # Number of trains currently running
		Trains_Number : Natural := 0;

	end Track_Type;

	type Tracks_Array is array (Positive range <>) of access Track_Type;

	------------------------------------ Json -> Track functions ----------------------
	-- Methods used to load Track data from a json configuration file

	function Get_Track_Array(File_Name : String) return access Tracks_Array;

	-- #
	-- # A Simple Print procedure, used for debugging purposes
	-- #
	procedure Print(Track : access Track_Type);

private

	function Get_Track(Json_Track : Json_Value) return access Track_Type;

	function Get_Track_Array(Json_v : Json_Value) return access Tracks_Array;

end Track;
