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
with Queue;

package Segment is

	package Train_Queue_Package is new Queue(Element => Positive);
	use Train_Queue_Package;

	-- # Exception raised when a Train attempts to access a Segment
	-- # from a Station different from First_End or Second_End.
	Bad_Segment_Access_Request_Exception : exception;


	-- #
	-- # This protected type is used to perform Train's access priority.
	-- # The main idea is to let tasks enqueue to the proper entry (Gain_Access_FB
	-- # or Gain_Access_Regional) and to subordinate tasks waiting on Gain_Access_Regional
	-- # execution to Gain_Access_FB queue status.
	-- #
	protected type Priority_Access_Controller is

		-- #
		-- # Main entry used to gain access.
		-- #
    	entry Gain_Access(
    		Train_Index : in 	Positive);

		-- #
		-- # Procedure used to free the resource.
		-- #
    	procedure Access_Gained;

    private

		-- #
		-- # Entry used to enqueue FB Trains
		-- #
    	entry Gain_Access_FB(
    		Train_Index : in 	Positive);

		-- #
		-- # Entry used to enqueue Regional Trains.
		-- # A task will access this entry only if the
		-- # resource is Free and no tasks will be waiting on
		-- # Gain_Access_FB entry.
		-- #
    	entry Gain_Access_Regional(
    		Train_Index : in 	Positive);

		-- # Status of the resource.
		Free : Boolean := True;

    end Priority_Access_Controller;

	-- #
	-- # This protected type allows to control the effective access
	-- # of a Train into the Segment. A Train first needs to be
	-- # added to the internal Trains_Order queue, because Enter attempts
	-- # will be ordered following the queue's order.
	-- #
	protected type Segment_Access_Controller(
		Id 					: Integer;
		Segment_Max_Speed 	: Positive;
		Segment_Length		: Positive;
		First_End 			: Positive;
		Second_End 			: Positive;
		-- # Maximum number of entrance per side
		Max 				: Natural)
	is

		-- #
		-- # Trains ask to Enter the Segment
		-- #
		entry Leave(Train_D : in Positive);

		-- #
		-- # Trains ask to Enter the Segment
		-- #
		entry Enter(
			To_Add 		: in	 Positive;
			Max_Speed 	: 	 out Positive;
			Leg_Length 	:	 out Positive);

		-- #
		-- # Procedure used to add a Train into Trains_Order queue.
		-- # The queue will maintain the access order of the Trains.
		-- #
		procedure Add_Train(
			Train_Index : in Positive);

	private

		-- #
		-- # Trains ask to Enter the Segment
		-- #
		entry Perform_Enter(
			To_Add 		: in	 Positive;
			Max_Speed 	: 	 out Positive;
			Leg_Length 	:	 out Positive);

		-- #
		-- # Entry used to enqueue Trains trying to enter
		-- # without being at the beginning of Trains_Queue.
		-- #
		entry Retry(
			To_Add 		: in	 Positive;
			Max_Speed 	: 	 out Positive;
			Leg_Length 	:	 out Positive);

		-- #
		-- # Entry used to make Train threads wait
		-- # at First End.
		-- #
		entry Retry_First_End(
			To_Add 		:	in 		Positive;
			Max_Speed 	: 	 	out Positive;
			Leg_Length 	:		out	Positive);

		-- #
		-- # Entry used to make Train threads wait
		-- # at Second End.
		-- #
		entry Retry_Second_End(
			To_Add 		:	in 		Positive;
			Max_Speed 	: 	 	out Positive;
			Leg_Length 	:		out	Positive);

		-- #
		-- # Private Entry used to enqueue trains, to guarantee an exit order.
		-- #
		entry Retry_Leave(
			Train_D : in Positive);

		-- # Tells weather a train is already running or not
		Free 				: Boolean := True;

		-- # Current maximum speed
		Current_Max_Speed 	: Positive := Segment_Max_Speed;

		-- # Current direction. Is set to 0 when the Segment is free
		Current_Direction 	: Natural := 0;

		-- # Queue of all the running trains
		Running_Trains 		: Unlimited_Simple_Queue;

		-- # Boolean guard telling if a train can retry to Leave the Segment
		Can_Retry_Leave 	: Boolean := False;

		-- # Number of exit attempt
		Enter_Retry_Num 	: Natural := 0;

		-- # Number of exit attempt
		Exit_Retry_Num 			: Natural := 0;

		-- # Number of trains currently running
		Trains_Number 		: Natural := 0;

		-- # Number of trains entered per direction
		Train_Entered_Per_Direction	: Natural := 0;

		-- # Guard which tells whether a Train can enter from
		-- # first End or not.
		Can_Enter_First_End : Boolean := False;

		-- # Guard which tells whether a Train can enter from
		-- # first End or not.
		Can_Enter_Second_End : Boolean := False;

		-- # Queue used to maintain the access order of the Trains.
		Trains_Order : Unlimited_Simple_Queue;

		-- # Number of tasks re-trying to enter (in order!)
		Retry_Num 	: Natural := 0;

		-- # Boolean guard used to regulate entrance.
		Can_Retry 	: Boolean := False;

	end Segment_Access_Controller;

	-- #
	-- # Tagged type used to encapsulate the entire access protocol.
	-- # it maintains a Priority_Access_Controller object and a Segment_Access_Controller
	-- # object, and uses them to perform multiple controlled access.
	-- #
	type Segment_Type is tagged limited private;

	-- #
	-- # This method allows a Train task to try to Enter into the
	-- # Segment.
	-- #
	procedure Enter(
		This		: access Segment_Type;
		To_Add 		: in	 Positive;
		Max_Speed 	: 	 out Positive;
		Leg_Length 	:	 out Positive);

	-- #
	-- # Method used to perform Leaving operation from the Segment.
	-- #
	procedure Leave(
		This		: access Segment_Type;
		Train_D 	: in 	 Positive);

	-- #
	-- # Simple function which returns the ID of the Segment.
	-- #
	function Id (
		This		: access Segment_Type) return Natural;


	-- # Array of Segments.
	type Segments_Array is array (Positive range <>) of access Segment_Type;

	------------------ Json -> Segment functions ----------------------

	-- #
	-- # Methods used to load Segment data from a json configuration file
	-- #
	function Get_Segment_Array(File_Name : String) return access Segments_Array;

	function Get_Segment(Json_Segment : Json_Value) return access Segment_Type;

private

	-- # Private fields definition for Segment_Type.
	type Segment_Type is tagged limited record
		Access_Controller 	: Priority_Access_Controller;
		Segment 			: access Segment_Access_Controller;
	end record;

	function Get_Segment_Array(Json_v : Json_Value) return access Segments_Array;

end Segment;
