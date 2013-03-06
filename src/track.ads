--==============================================================================
-- Created by:
--		Moreno Ambrosin
--  	Mat.  : 1035635
-- Date:
-- 		09/02/2013
-- Updates:
-- 		05/03/2013 : Started to implement multiple-running tracks
--==============================================================================

with Train;use Train;
with Gnatcoll.JSON;use Gnatcoll.JSON;
with JSON_Helper;use JSON_Helper;

package Track is

	-- # IDs of currently traveling trains
	type Train_Queue is array (Positive range <>) of Integer;

	protected type Track_Type(
		Id 				: Natural;
		Track_Max_Speed : Positive;
		-- Length expressed in meters
		Track_Length	: Positive;
		Queue_Dim 		: Positive;
		First_End 		: Positive;
		Second_End 		: Positive)
	is

		-- #
		-- # Trains ask to Enter the track; the access is in mutual-exclusion
		-- #
		entry Leave(T : in Train_Descriptor);

		-- #
		-- # Trains ask to Enter the track; the access is in mutual-exclusion
		-- #
		entry Enter(To_Add :  in out Train_Descriptor; Max_Speed : out Positive;Leg_Length : out Positive);

	private

		-- #
		-- # Private Entry used to enqueue trains whose direction are not the same
		-- # as the direction of already running trains.
		-- #
		entry Wait(To_Add :  in out Train_Descriptor; Max_Speed : out Positive;Leg_Length : out Positive);

		-- #
		-- # Private Entry used to enqueue trains, to guarantee an exit order.
		-- #
		entry Retry(T : Train_Descriptor);

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
