with Track;
with Generic_Station;
with Regional_Station;
with Traveler;
with Move_Operation;

package Environment Is

	-- Creation of Regional Stations
	Stations : Generic_Station.Stations_array_Ref := Regional_Station.GetRegionalStationarray("res/stations.json");

	-- array of Travelers
    Travelers : Traveler.Traveler_Manager_array := Traveler.Get_Traveler_Manager_array("res/travelers.json");

    Operations : array (1 .. 4) of Traveler.Traveler_Operations(1 .. 2) := (
		-- Operations for Traveler1
		1 =>  (	1 => new Move_Operation.Move_Operation_Type'(Manager => Travelers(1)'Access),
		        2 => new Move_Operation.Move_Operation_Type'(Manager => Travelers(1)'Access)),
		2 =>  (	1 => new Move_Operation.Move_Operation_Type'(Manager => Travelers(2)'Access),
		        2 => new Move_Operation.Move_Operation_Type'(Manager => Travelers(2)'Access)),
		3 =>  (	1 => new Move_Operation.Move_Operation_Type'(Manager => Travelers(3)'Access),
		        2 => new Move_Operation.Move_Operation_Type'(Manager => Travelers(3)'Access)),
		4 =>  (	1 => new Move_Operation.Move_Operation_Type'(Manager => Travelers(4)'Access),
		        2 => new Move_Operation.Move_Operation_Type'(Manager => Travelers(4)'Access))
    );

    Tracks : array (1 .. 4) Of Track.Track_Type;

end Environment;
