with Railway.Station;use Railway.Station;
with Railway.Train;use Railway.Train;

package body Regional_Station is
	
	-- Definition of Inherited Methods --
	procedure Enter(Station : Regional_Station_Type ; Descriptor : in out Train_Descriptor;Plattform : Integer) is 
	begin 
		Station.Plattforms(Plattform).Enter(Descriptor);
	end Enter;

	procedure Leave(Station : Regional_Station_Type ; Descriptor : in out Train_Descriptor;Plattform : Integer) is 
	begin 
		Station.Plattforms(Plattform).Leave(Descriptor);
	end Leave;
		
end Regional_Station; 
