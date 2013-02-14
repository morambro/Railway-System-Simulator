--==============================================================================
-- File:
--	passenger.ads
-- Created by:
--		Moreno Ambrosin
--  	Mat.  : 1035635
-- Date:
-- 	13/02/2013
--==============================================================================

with Ada.Strings.Unbounded;
with Generic_Operation_Interface;use Generic_Operation_Interface;

package Passenger is
	
	package Unbounded_Strings renames Ada.Strings.Unbounded;
	
	type Passenger_Operations is Array(Positive range <>) of Any_Operation;
	
	-- Passenger tagged type declaration
	type Passenger_Type(Operation_Number : Positive) is tagged private;
		
		function GetID(P : Passenger_Type) 		return Integer;
		function GetName(P : Passenger_Type) 	return String;
		function GetSurname(P : Passenger_Type) return String;
		function GetOperations(P : Passenger_Type) return Passenger_Operations;
	
	type Passenger_Ref is access all Passenger_Type;
	
	function NewPassenger(Operations_Number : Positive; ID : Integer; Name : String; Surname : String) return Passenger_Ref;
	
private 
	
	type Passenger_Type(Operation_Number : Positive) is tagged record
		ID 			: Integer;
		Name 		: Unbounded_Strings.Unbounded_String;
		Surname 	: Unbounded_Strings.Unbounded_String;
		Operations	: Passenger_Operations(1..Operation_Number);
	end record;

end Passenger;
