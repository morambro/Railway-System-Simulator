with Track;
with Generic_Station;
with Regional_Station;
with Traveler;
with Move_Operation;
with Ada.Strings.Unbounded;

package Environment Is

    package Unbounded_Strings renames Ada.Strings.Unbounded;

    use Unbounded_Strings;

	-- Stations Creation
	Stations : Regional_Station.Stations_array_Ref := Regional_Station.GetRegionalStationarray("res/stations.json");

	-- array of Travelers
    Travelers : Traveler.Traveler_Manager_array := Traveler.Get_Traveler_Manager_array("res/travelers.json");

    Operations : array (1 .. 4) of Traveler.Traveler_Operations(1 .. 2) := (
		-- Operations for Traveler1
		1 =>  (	1 => new Move_Operation.Move_Operation_Type(Travelers(1)'Access),
		        2 => new Move_Operation.Move_Operation_Type(Travelers(1)'Access)),
		2 =>  (	1 => new Move_Operation.Move_Operation_Type(Travelers(2)'Access),
		        2 => new Move_Operation.Move_Operation_Type(Travelers(2)'Access)),
		3 =>  (	1 => new Move_Operation.Move_Operation_Type(Travelers(3)'Access),
		        2 => new Move_Operation.Move_Operation_Type(Travelers(3)'Access)),
		4 =>  (	1 => new Move_Operation.Move_Operation_Type(Travelers(4)'Access),
		        2 => new Move_Operation.Move_Operation_Type(Travelers(4)'Access))
    );


    Tracks : array (1 .. 4) Of Track.Track_Type;

end Environment;
