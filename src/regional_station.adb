with Ada.Text_IO;use Ada.Text_IO;
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
	
	
	procedure WaitForTrain(
			Station : Regional_Station_Type;
			Traveler : in out Traveler_Manager;
			Plattform : Integer) is
	begin
		Station.Plattforms(Plattform).AddOutgoingTraveler(Traveler);
		Station.Panel.SetStatus("User " & GetName(Traveler) & " entered plattform " & Integer'Image(Plattform));
	end WaitForTrain;
	
	-- 
	-- Creates a new Station instance
	-- @return: A reference of the new created Station
	--
	function NewRegionalStation(
		Plattforms_Number : Positive;
		Name : Positive) return Station_Ref 
	is
		Station : access Regional_Station_Type:= new Regional_Station_Type(Plattforms_Number);
	begin
		Station.Name := Name;
		for I in Positive range 1..Plattforms_Number loop
			Station.Plattforms(I) := new Plattform_Type(I);
		end loop;
		Station.Panel := new Notice_Panel_Entity(Name);
		return Station;
	end;	
	
end Regional_Station; 
