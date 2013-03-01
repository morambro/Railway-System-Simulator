with Generic_Station;use Generic_Station;
with Train;
with Plattform;
with Traveler;
with Notice_Panel;
with Gnatcoll.JSON;use Gnatcoll.JSON;
with JSON_Helper;use JSON_Helper;

with Ada.Unchecked_Deallocation;

package Regional_Station is

	-- Array Containing plattforms references
	type Plattforms_List is array (Positive range <>) of access Plattform.Plattform_Type;

	-- Definition of Regional Station Type implementing Station_Interface --
	type Regional_Station_Type(Plattforms_Number : Positive) is new Station_Interface with private;

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
		Name : Positive) return Station_Ref;

	type Stations_Array_Ref is access Stations_Array;

	procedure Print(This : Regional_Station_Type);

	function GetRegionalStation(Json_Station : Json_Value) return Station_Ref;

	function GetRegionalStationArray(Json_Station : Json_Value) return Stations_Array_Ref;

	function GetRegionalStationArray(Json_Station : String) return Stations_Array_Ref;

--  	procedure Free_Station_Array is new Ada.Unchecked_Deallocation
--      	(Object => Stations_Array, Name => Stations_Array_Ref);


	function GetRegionalStationArray(Size : Integer) return Stations_Array;

private

	type Regional_Station_Type(Plattforms_Number : Positive) is new Station_Interface with
	record
		Name : Positive;
		Plattforms : Plattforms_List(1..Plattforms_Number);
		Panel : access Notice_Panel.Notice_Panel_Entity;
	end record;

end Regional_Station;
