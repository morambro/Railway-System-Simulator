with Railway.Train;use Railway.Train;

package body Railway.Station is

	protected body Plattform is
		entry Enter(Descriptor : in out Train_Descriptor) when Free = True is
		begin 
			Free := False;
		end Enter;
		
		procedure Leave(Descriptor : in out Train_Descriptor) is
		begin 
			Free := True;
		end Leave;
	end Plattform;


	
end Railway.Station;
