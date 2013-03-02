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
		-- # Tells weather a train is already running or not
		Free : Boolean := True;
		-- # Maximum Speed at which a Train can run
		Track_Max_Speed : Positive := 200;
		-- # Track Length in km
		Track_Length : Float := 10.1;

	end Track_Type;

end Track;
