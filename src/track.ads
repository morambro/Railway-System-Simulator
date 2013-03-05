with Train;use Train;

package Track is

	-- # IDs of currently traveling trains
	type Train_Queue is array (Positive range <>) of Integer;

	protected type Track_Type is

		-- #
		-- # Trains ask to Enter the track; the access is in mutual-exclusion
		-- #
		entry Leave(T : in Train_Descriptor);

		-- #
		-- # Trains ask to Enter the track; the access is in mutual-exclusion
		-- #
		entry Enter(To_Add :  in out Train_Descriptor; Max_Speed : out Positive;Leg_Length : out Float);

	private

		-- #
		-- # Private Entry used to enqueue trains whose direction are not the same
		-- # as the direction of already running trains
		-- #
		entry Wait(To_Add :  in out Train_Descriptor; Max_Speed : out Positive;Leg_Length : out Float);

		entry Retry(T : Train_Descriptor);

		-- # Tells weather a train is already running or not
		Free : Boolean := True;
		-- # Maximum Speed at which a Train can run
		Track_Max_Speed : Positive := 200;
		-- # Track Length in km
		Track_Length : Float := 10.1;

		-- # Current direction. Is set to 0 when the track is free
		Current_Direction : Natural := 0;

		-- # Queue of all the running trains
		Running_Trains : Train_Queue (1..10);

		Can_Retry_Leave : Boolean := False;

		Can_Retry_Enter : Boolean := False;

		Retry_Num : Integer := 0;

		Trains_Number : Natural := 0;

	end Track_Type;


end Track;
