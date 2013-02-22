with Track;use Track;
with Generic_Station;use Generic_Station;
with Regional_Station;use Regional_Station;
with Task_Pool;use Task_Pool;
with Traveler;use Traveler;
with Move_Operation;use Move_Operation;
with Ada.Strings.Unbounded;

package Environment is
	
	package Unbounded_Strings renames Ada.Strings.Unbounded;
	use Unbounded_Strings;

	-- Creation of Actors for Travelers
	Traveler_Tasks : Task_Pool_Type(5);

	-- Creation of 5 stations
	Stations : array (1 .. 5) of Station_Ref := (
		1 => NewRegionalStation(4,12211),
		2 => NewRegionalStation(3,44556),
		3 => NewRegionalStation(2,32111),
		4 => NewRegionalStation(3,66442),
		5 => NewRegionalStation(2,56655)
	);


	
	Traveler1_Manager : aliased Traveler_Manager := (
		Traveler => (2,To_Unbounded_String("Sergio"),To_Unbounded_String("Rossi")),
		Next_Operation => 1,
		Destination => 1
	); 
	
	
	
	-- Operations for Traveler1
	Traveler1_Operations : Traveler_Operations(1..2) := (
		1 => new Move_Operation_Type(Traveler1_Manager'Access),
		2 => new Move_Operation_Type(Traveler1_Manager'Access)
	);
	
	Track_1 : Track_Type;
	
	
end Environment;
