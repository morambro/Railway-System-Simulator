With Track;
With Generic_Station;
With Regional_Station;
With Traveler;
With Move_Operation;
With Ada.Strings.Unbounded;
with JSON_Helper;use JSON_Helper;


with Gnatcoll.JSON;use Gnatcoll.JSON;


Package Environment Is

    Package Unbounded_Strings Renames Ada.Strings.Unbounded;

    Use Unbounded_Strings;

	-- Creation of 5 stations
    Stations : Generic_Station.Stations_Array(1 .. 5) := (
		1 => Regional_Station.GetRegionalStation(GetJsonValue("res/station.json").Get("station")),
		2 => Regional_Station.GetRegionalStation(GetJsonValue("res/station.json").Get("station")),
		3 => Regional_Station.GetRegionalStation(GetJsonValue("res/station.json").Get("station")),
		4 => Regional_Station.GetRegionalStation(GetJsonValue("res/station.json").Get("station")),
		5 => Regional_Station.GetRegionalStation(GetJsonValue("res/station.json").Get("station"))
	);

--  	Stations2 : access Generic_Station.Stations_Array :=
--  		Regional_Station.GetRegionalStationArray(GetJsonValue("res/stations.json"));


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
