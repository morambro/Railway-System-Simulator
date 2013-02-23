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

	Travelers : array (1 .. 4) of aliased Traveler_Manager := (
		1 	=> (Traveler => (2,To_Unbounded_String("Sergio"),To_Unbounded_String("Rossi")),
				Next_Operation => 1,
				Destination => 1),
		2 	=> (Traveler => (2,To_Unbounded_String("Mario"),To_Unbounded_String("Verdi")),
				Next_Operation => 1,
				Destination => 1),
		3 	=> (Traveler => (2,To_Unbounded_String("Roberto"),To_Unbounded_String("Bianchi")),
				Next_Operation => 1,
				Destination => 1),
		4 	=> (Traveler => (2,To_Unbounded_String("John"),To_Unbounded_String("Doe")),
				Next_Operation => 1,
				Destination => 1)
	);
	

	Operations : array (1 .. 4) of Traveler_Operations(1 .. 2) := (
	-- Operations for Traveler1
		1 =>  (	1 => new Move_Operation_Type(Travelers(1)'Access),
				2 => new Move_Operation_Type(Travelers(1)'Access)),
		2 =>  (	1 => new Move_Operation_Type(Travelers(2)'Access),
				2 => new Move_Operation_Type(Travelers(2)'Access)),
		3 =>  (	1 => new Move_Operation_Type(Travelers(3)'Access),
				2 => new Move_Operation_Type(Travelers(3)'Access)),
		4 =>  (	1 => new Move_Operation_Type(Travelers(4)'Access),
				2 => new Move_Operation_Type(Travelers(4)'Access))
	);
	
	
	Tracks : array (1 .. 5) of Track_Type;
	
	
end Environment;
