--==============================================================================
-- File:
--	traveler.ads
-- Created by:
--		Moreno Ambrosin
--  	Mat.  : 1035635
-- Date:
-- 	13/02/2013
--==============================================================================

with Ada.Strings.Unbounded;
with Generic_Operation_Interface;use Generic_Operation_Interface;

package Traveler is
	
	package Unbounded_Strings renames Ada.Strings.Unbounded;
	
	type Traveler_Operations is Array(Positive range <>) of Any_Operation;
	
	-- Traveler type declaration
	type Traveler_Type is record
		ID 			: Integer;
		Name 		: Unbounded_Strings.Unbounded_String;
		Surname 	: Unbounded_Strings.Unbounded_String;
	end record;

	type Traveler_Manager is record
		Traveler 		: Traveler_Type;
		Next_Operation 	: Positive := 1;
		Destination 	: Positive := 1;
	end record;	
	
	function GetName(Traveler : Traveler_Manager) return String;
	
end Traveler;
