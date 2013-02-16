with Railway.Track;use Railway.Track;
with Railway.Station;use Railway.Station;
with Regional_Station;use Regional_Station;
with Task_Pool;use Task_Pool;
with Passenger;use Passenger;
with Passenger.Move_Operation;use Passenger.Move_Operation;
with Ada.Strings.Unbounded;

package Environment is
	
	package Unbounded_Strings renames Ada.Strings.Unbounded;
	use Unbounded_Strings;

	Passenger1 : aliased Passenger_Type := (
		1,
		To_Unbounded_String("Moreno"),
		To_Unbounded_String("Ambrosin")
	);
	
	Passenger2 : aliased Passenger_Type := (
		2,
		To_Unbounded_String("Sergio"),
		To_Unbounded_String("Rossi")
	);
	
	-- Operations for Passenger1
	Passenger1_Operations : Passenger_Operations(1..2) := (
		1 => new Move_Operation_Type(Passenger1'Access),
		2 => new Move_Operation_Type(Passenger1'Access)
	);
	
	Station1 : Station_Ref := new Regional_Station_Type(3);-- NewRegionalStation(2,13422);
	Station2 : Station_Ref := new Regional_Station_Type(2);
	
	
	Tasks : Task_Pool_Type(5);
	
	Track_1 : Track;
	
end Environment;
