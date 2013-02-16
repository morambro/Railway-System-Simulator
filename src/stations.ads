with Regional_Station;use Regional_Station;
with Generic_Station;use Generic_Station;

package Stations is
	
	type All_Stations is array (Positive range <>) of Station_Ref;
	
	-- Array wich will contain all stations
	Stations : All_Stations(1..3) := (
		1 => NewRegionalStation(2,1),
		2 => NewRegionalStation(2,1),
		3 => NewRegionalStation(2,1)
	);
	
end Stations;
