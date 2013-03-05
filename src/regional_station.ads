--==============================================================================
-- Created by:
--		Moreno Ambrosin
--  	Mat.  : 1035635
-- Date:
-- 		09/02/2013
--==============================================================================

with Generic_Station;use Generic_Station;
with Train;
with Platform;
with Traveler;
with Notice_Panel;
with Gnatcoll.JSON;use Gnatcoll.JSON;
with JSON_Helper;use JSON_Helper;
with Ada.Strings.Unbounded;

with Ada.Finalization;
with Unchecked_Deallocation;

package Regional_Station is

	package Unbounded_Strings renames Ada.Strings.Unbounded;

	use Unbounded_Strings;


	-- # Array Containing Platforms references
	type Platforms_List is array (Positive range <>) of access Platform.Platform_Type;

	type Platform_Booking is array (Positive range <>) of Boolean;

	-- # Definition of Regional Station Type implementing Station_Interface --
	type Regional_Station_Type(Platforms_Number : Positive) is
		new Ada.Finalization.Controlled
		and Station_Interface
	with private;

		overriding procedure Enter(
			This : Regional_Station_Type;
			Descriptor : in out Train.Train_Descriptor;
			Platform: Integer);

		overriding procedure Leave(
			This : Regional_Station_Type;
			Descriptor : in out Train.Train_Descriptor;
			Platform: Integer);

		overriding procedure WaitForTrain(
			This : Regional_Station_Type;
			Incoming_Traveler : in out Traveler.Traveler_Manager;
			Platform: Integer);

	function New_Regional_Station(
		Platforms_Number : Positive;
		Name : String) return Station_Ref;

	procedure Print(This : Regional_Station_Type);

	function Get_Regional_Station_Array(Json_Station : String) return Stations_Array_Ref;

	overriding procedure Finalize   (This: in out Regional_Station_Type);

private

	type Regional_Station_Type(Platforms_Number : Positive) is
		new Ada.Finalization.Controlled and
		Station_Interface
	with record
		Name : Unbounded_Strings.Unbounded_String;
		Platforms : Platforms_List(1..Platforms_Number);
		Panel : access Notice_Panel.Notice_Panel_Entity := null;
		-- Indicates for each platform if it is free or not
		Platform_Free : Platform_Booking(1 .. Platforms_Number) := (others => true);
	end record;

	function Get_Regional_Station(Json_Station : Json_Value) return Station_Ref;

	function Get_Regional_Station_Array(Json_v : Json_Value) return Stations_Array_Ref;

end Regional_Station;
