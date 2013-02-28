With Track;
With Generic_Station;
With Regional_Station;
With Traveler;
With Move_Operation;
With Ada.Strings.Unbounded;

Package Environment Is

    Package Unbounded_Strings Renames Ada.Strings.Unbounded;

    Use Unbounded_Strings;

	-- Creation of 5 stations
    Stations : Array (1 .. 5) Of Generic_Station.Station_Ref := (
		1 => Regional_Station.NewRegionalStation(4,12211),
		2 => Regional_Station.NewRegionalStation(3,44556),
		3 => Regional_Station.NewRegionalStation(2,32111),
		4 => Regional_Station.NewRegionalStation(3,66442),
		5 => Regional_Station.NewRegionalStation(2,56655)
	);

	-- Array of Travelers
    Travelers : Traveler.Traveler_Manager_Array := Traveler.Get_Traveler_Manager_Array("res/travelers.json");


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


    Tracks : Array (1 .. 4) Of Track.Track_Type;

end Environment;
