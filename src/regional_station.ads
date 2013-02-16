with Railway.Station;use Railway.Station;
with Railway.Train;use Railway.Train;
with Plattform; use Plattform;

package Regional_Station is
	
	-- Array Containing plattforms references
	type Plattforms_List is array (Positive range <>) of access Plattform_Type;
	
	-- Definition of Regional Station Type implementing Station_Interface --
	type Regional_Station_Type(Plattforms_Number : Positive) is new Station_Interface with private;
	
		overriding procedure Enter(
			Station : Regional_Station_Type; 
			Descriptor : in out Train_Descriptor;
			Plattform : Integer);
		
		overriding procedure Leave(
			Station : Regional_Station_Type;
			Descriptor : in out Train_Descriptor;
			Plattform : Integer);
	
	function NewRegionalStation(
		Plattforms_Number : Positive;
		Name : Positive) return Station_Ref;

private 
	
	type Regional_Station_Type(Plattforms_Number : Positive) is new Station_Interface with
	record
		
		Name : Positive;
		Plattforms : Plattforms_List(1..Plattforms_Number);
		
	end record;
	
end Regional_Station; 
