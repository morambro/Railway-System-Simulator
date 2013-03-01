with Generic_Station;use Generic_Station;
with Train;
with Plattform;
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


	-- Array Containing plattforms references
	type Plattforms_List is array (Positive range <>) of access Plattform.Plattform_Type;

	-- Definition of Regional Station Type implementing Station_Interface --
	type Regional_Station_Type(Plattforms_Number : Positive) is 
		new Ada.Finalization.Controlled 
		and Station_Interface 
	with private;

		overriding procedure Enter(
			This : Regional_Station_Type;
			Descriptor : in out Train.Train_Descriptor;
			Plattform : Integer);

		overriding procedure Leave(
			This : Regional_Station_Type;
			Descriptor : in out Train.Train_Descriptor;
			Plattform : Integer);

		overriding procedure WaitForTrain(
			This : Regional_Station_Type;
			Incoming_Traveler : in out Traveler.Traveler_Manager;
			Plattform : Integer);

	function NewRegionalStation(
		Plattforms_Number : Positive;
		Name : String) return Station_Ref;

	procedure Print(This : Regional_Station_Type);

	function GetRegionalStationArray(Json_Station : String) return Stations_Array_Ref;

	overriding procedure Finalize   (This: in out Regional_Station_Type);

private

	type Regional_Station_Type(Plattforms_Number : Positive) is
		new Ada.Finalization.Controlled and
		Station_Interface
	with record
		Name : Unbounded_Strings.Unbounded_String;
		Plattforms : Plattforms_List(1..Plattforms_Number);
		Panel : access Notice_Panel.Notice_Panel_Entity := null;
	end record;

	function GetRegionalStation(Json_Station : Json_Value) return Station_Ref;

	function GetRegionalStationArray(Json_v : Json_Value) return Stations_Array_Ref;

end Regional_Station;
