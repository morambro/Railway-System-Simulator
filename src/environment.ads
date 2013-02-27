With Track;Use Track;
With Generic_Station;Use Generic_Station;
With Regional_Station;
With Task_Pool;Use Task_Pool;
With Traveler;Use Traveler;
With Move_Operation;Use Move_Operation;
With Ada.Strings.Unbounded;

Package Environment Is

    Package Unbounded_Strings Renames Ada.Strings.Unbounded;

    Use Unbounded_Strings;

	-- Creation of Actors for Travelers
	Traveler_Tasks : Task_Pool_Type(5);

	-- Creation of 5 stations
    Stations : Array (1 .. 5) Of Station_Ref := (
		1 => Regional_Station.NewRegionalStation(4,12211),
		2 => Regional_Station.NewRegionalStation(3,44556),
		3 => Regional_Station.NewRegionalStation(2,32111),
		4 => Regional_Station.NewRegionalStation(3,66442),
		5 => Regional_Station.NewRegionalStation(2,56655)
	);

    Travelers : Array (1 .. 4) Of Aliased Traveler_Manager := (
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


    Tracks : Array (1 .. 4) Of Track_Type;

end Environment;
