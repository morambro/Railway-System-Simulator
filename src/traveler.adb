package body Traveler is
	
	function GetName(Traveler : Traveler_Manager) return String is
	begin 
		return Unbounded_Strings.To_String(Traveler.Traveler.Name);
	end GetName;

end Traveler;
