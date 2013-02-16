with Train;use Train;
with Traveler;use Traveler;

package Generic_Station is

	---------------------------------- STATION INTERFACE --------------------------------------
	type Station_Interface is interface;

		-- Station_Interface Methods
		procedure Enter(
			Station : Station_Interface; 
			Descriptor : in out Train_Descriptor;
			Plattform : Integer) 
		is abstract;
	
		procedure Leave(
			Station : Station_Interface;
			Descriptor : in out Train_Descriptor;
			Plattform : Integer) 
		is abstract;
		
		procedure WaitForTrain(
			Station : Station_Interface;
			Traveler : in out Traveler_Manager;
			Plattform : Integer)
		is abstract;

	-- End Of the Interface
	
   	-- generic Station reference type to be used inside records: Type'Class doesn't
   	-- have a fixed size so it can be not allocated inside a record.
   	type Station_Ref is access all Station_Interface'Class;

	
end Generic_Station;
