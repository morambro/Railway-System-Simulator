with Railway.Track;use Railway.Track;
with Railway.Station;use Railway.Station;
with Regional_Station;use Regional_Station;
with Task_Pool;use Task_Pool;
with Traveler;use Traveler;
with Move_Operation;use Move_Operation;
with Ada.Strings.Unbounded;

package Environment is
	
	package Unbounded_Strings renames Ada.Strings.Unbounded;
	use Unbounded_Strings;

	Tasks : Task_Pool_Type(5);

	Traveler1 : Traveler_Type := (
		1,
		To_Unbounded_String("Moreno"),
		To_Unbounded_String("Ambrosin")
	);
	
	
	Traveler1_Manager : aliased Traveler_Manager := (
		Traveler => Traveler1,--(2,To_Unbounded_String("Sergio"),To_Unbounded_String("Rossi")),
		Next_Operation => 1,
		Destination => 1
	); 
	
	
	-- Operations for Traveler1
	Traveler1_Operations : Traveler_Operations(1..2) := (
		1 => new Move_Operation_Type(Traveler1_Manager'Access),
		2 => new Move_Operation_Type(Traveler1_Manager'Access)
	);
	
	
	type All_Stations is array (Positive range <>) of Station_Ref;
	
	-- Array wich will contain all stations
	Stations : All_Stations(1..3) := (
		1 => new Regional_Station_Type(3),
		2 => new Regional_Station_Type(2),
		3 => new Regional_Station_Type(2)
	);
	
	Track_1 : Track;
	
end Environment;
