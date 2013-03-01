with Ada.Text_IO;use Ada.Text_IO;

package body Regional_Station is

	-- Definition of Inherited Methods --
	procedure Enter(
		This : Regional_Station_Type;
		Descriptor : in out Train.Train_Descriptor;
		Plattform : Integer) is
	begin
		This.Plattforms(Plattform).Enter(Descriptor);
	end Enter;

	procedure Leave(
		This : Regional_Station_Type;
		Descriptor : in out Train.Train_Descriptor;
		Plattform : Integer) is
	begin
		This.Plattforms(Plattform).Leave(Descriptor);
	end Leave;

	--
	-- Procedure called by a Traveler to enqueue at a given Platform
	-- waiting for a specific Train
	--
	--
	procedure WaitForTrain(
			This 	: Regional_Station_Type;
			Incoming_Traveler 	: in out Traveler.Traveler_Manager;
			Plattform 	: Integer) is
	begin
		This.Plattforms(Plattform).AddOutgoingTraveler(Incoming_Traveler);
		This.Panel.SetStatus(
			"User " & Traveler.GetName(Incoming_Traveler) & " entered platform " & Integer'Image(Plattform));
	end WaitForTrain;

	--
	-- Creates a new Station instance
	--
	-- @return: A reference of the new created Station
	--
	function NewRegionalStation(
		Plattforms_Number : Positive;
		Name : Positive) return Station_Ref
	is
		Station : access Regional_Station_Type := new Regional_Station_Type(Plattforms_Number);
	begin
		Station.Name := Name;
		for I in Positive range 1..Plattforms_Number loop
			Station.Plattforms(I) := new Plattform.Plattform_Type(I);
		end loop;
		Station.Panel := new Notice_Panel.Notice_Panel_Entity(Name);
		return Station;
	end;

	procedure Print(This : Regional_Station_Type) is
	begin
		Put_Line ("Name : " & Integer'Image(This.Name));
		Put_Line ("Platform Number : " & Integer'Image(This.Plattforms_Number));
    end Print;

-------------------------------------- JSON - Regional Station --------------------------------------

	function GetRegionalStation(Json_Station : Json_Value) return Station_Ref
	is
		Platforms_Number : Positive := Json_Station.Get("plattform_number");
		Name : Positive 			 := Json_Station.Get("name");
	begin
		return NewRegionalStation(Platforms_Number,Name);
	end;

	function GetRegionalStationArray(Json_v : Json_Value) return Stations_Array_Ref is
		J_Array : JSON_Array := Json_v.Get(Field => "stations");
		Array_Length : constant Natural := Length (J_Array);
		T : Stations_Array_Ref := new Stations_Array(1 .. Array_Length);
	begin

		for I in 1 .. T'Length loop
			T(I) := GetRegionalStation(Get(Arr => J_Array, Index => I));
		end loop;

		return T;
	end GetRegionalStationArray;


	function GetRegionalStationArray(Json_Station : String) return Stations_Array_Ref is
	begin
		return GetRegionalStationArray(GetJsonValue(Json_Station));
    end Getregionalstationarray;

end Regional_Station;
