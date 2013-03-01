with Train;
with Traveler;

with Unchecked_Deallocation;

with Ada.Finalization;

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

		procedure Print(This : Station_Interface) is abstract;
		

	-- End Of the Interface

   	-- generic Station reference type to be used inside records: Type'Class doesn't
   	-- have a fixed size so it can be not allocated inside a record.
   	type Station_Ref is access all Station_Interface'Class;

   	type Stations_Array is array (Positive range <>) of Station_Ref;
	
	type Stations_Array_Ref is access Stations_Array;

	-- Code to manage memory deallocation of a Station. All objects of type < Station_Interface
	-- will be deallocated by Free method.
 	procedure Free is new Unchecked_Deallocation (
      		Station_Interface'Class,
			Station_Ref
	);

	pragma Controlled (Station_Ref);


end Generic_Station;
