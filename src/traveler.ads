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
with JSON_Helper;
with Gnatcoll.JSON;use Gnatcoll.JSON;
with JSON_Helper;use JSON_Helper;

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

	type Traveler_Manager_Array is Array(Positive range <>) of aliased Traveler_Manager;

	function Get_Name(Traveler : Traveler_Manager) return String;

	procedure Print(T : Traveler_Manager);

	function Get_Traveler_Manager_Array(Json_Traveler : String) return Traveler_Manager_Array;

private

		--
	-- Creates a Traveler from JSON object
	--
	-- @return : A Traveler_Type object
	--
	function Get_Traveler(Json_Traveler : JSON_Value) return Traveler_Type;

	--
	-- Creates a Traveler_Manager from JSON object
	--
	-- @return : A Traveler_Manager object
	--
	function Get_Traveler_Manager(Json_Traveler : JSON_Value) return Traveler_Manager;

	--
	-- Creates a Traveler_Manager_Array from JSON object
	--
	-- @return : A Traveler_Manager_Array object
	--
	function Get_Traveler_Manager_Array(Json_Traveler : JSON_Value) return Traveler_Manager_Array;

end Traveler;
