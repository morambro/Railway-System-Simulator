--==============================================================================
-- File:
--	passenger.adb
-- Created by:
--		Moreno Ambrosin
--  	Mat.  : 1035635
-- Date:
-- 	13/02/2013
--==============================================================================

with Ada.Strings.Unbounded;
with Operation;use Operation;	

package body Passenger is
	
	
	function GetID(P : Passenger_Type) 		return Integer is
	begin
		return P.ID;
	end getID;
	
	function GetName(P : Passenger_Type) 	return String is
	begin
		return Unbounded_Strings.To_String(P.Name);
	end getName;
	
	function GetSurname(P : Passenger_Type) return String is
	begin
		return Unbounded_Strings.To_String(P.Surname);
	end getSurname;
	
	function GetOperations(P : Passenger_Type) return Passenger_Operations is
	begin
		return P.Operations;
	end;
	
	
	-- Constructor function definition
	function NewPassengerType(
			Operations_Number : Positive; 
			ID : Integer; 
			Name : String; 
			Surname : String) return Passenger_Type is
		
		New_Passenger : Passenger_Type(Operations_Number);
	
	begin
		New_Passenger.ID 		:= ID;
		New_Passenger.Name 		:= Unbounded_Strings.To_Unbounded_String(Name);
		New_Passenger.Surname 	:= Unbounded_Strings.To_Unbounded_String(Surname);
		for I in 1..Operations_Number loop
			New_Passenger.Operations(I) := NewOperation(Name);
		end loop;
		return New_Passenger;
	end;

end Passenger;
