with Train;
with Traveler;

package Generic_Station is

	---------------------------------- STATION INTERFACE --------------------------------------
	type Station_Interface is interface;

		-- Station_Interface Methods
		procedure Enter(
			This : Station_Interface;
			Descriptor : in out Train.Train_Descriptor;
			Plattform : Integer)
		is abstract;

		procedure Leave(
			This : Station_Interface;
			Descriptor : in out Train.Train_Descriptor;
			Plattform : Integer)
		is abstract;

		procedure WaitForTrain(
			This : Station_Interface;
			Incoming_Traveler : in out Traveler.Traveler_Manager;
			Plattform : Integer)
		is abstract;

	-- End Of the Interface

   	-- generic Station reference type to be used inside records: Type'Class doesn't
   	-- have a fixed size so it can be not allocated inside a record.
   	type Station_Ref is access all Station_Interface'Class;


end Generic_Station;
