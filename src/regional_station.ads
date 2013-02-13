with Railway.Station;use Railway.Station;
with Railway.Train;use Railway.Train;

package Regional_Station is
	
	type Plattform_Ref is access Plattform;
	
	type Plattforms_List is array (Positive range <>) of Plattform_Ref;
	
	-- Definition of Regional Station Type implementing Station_Interface --
	type Regional_Station_Type is new Station_Interface with private;
	
		overriding procedure Enter(Station : Regional_Station_Type ; Descriptor : in out Train_Descriptor;Plattform : Integer);
		
		overriding procedure Leave(Station : Regional_Station_Type ; Descriptor : in out Train_Descriptor;Plattform : Integer);
	

private 
	
	type Regional_Station_Type is new Station_Interface with
	record
		Plattforms : Plattforms_List(1..2) := (
			1 => new Plattform(1),
			2 => new Plattform(2)
		);
	end record;
	
end Regional_Station; 
