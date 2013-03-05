--==============================================================================
-- Created by:
--		Moreno Ambrosin
--  	Mat.  : 1035635
-- Date:
-- 		09/02/2013
--==============================================================================

with Ada.Text_IO;use Ada.Text_IO;

package body Regional_Station is

	-- # Definition of Inherited Methods --
	procedure Enter(
		This : Regional_Station_Type;
		Descriptor : in out Train.Train_Descriptor;
		Platform : Integer) is
	begin
		This.Platforms(Platform).Enter(Descriptor);
	end Enter;

	procedure Leave(
		This : Regional_Station_Type;
		Descriptor : in out Train.Train_Descriptor;
		Platform : Integer) is
	begin
		This.Platforms(Platform).Leave(Descriptor);
	end Leave;

	-- #
	-- # Procedure called by a Traveler to enqueue at a given Platform
	-- # waiting for a specific Train
	-- #
	-- #
	procedure WaitForTrain(
			This 	: Regional_Station_Type;
			Incoming_Traveler 	: in out Traveler.Traveler_Manager;
			Platform 	: Integer) is
	begin
		This.Platforms(Platform).AddOutgoingTraveler(Incoming_Traveler);
		This.Panel.SetStatus(
			"User " & Traveler.Get_Name(Incoming_Traveler) & " entered platform " & Integer'Image(Platform));
	end WaitForTrain;

	-- #
	-- # Creates a new Station instance
	-- #
	-- # @return: A reference of the new created Station
	-- #
	function New_Regional_Station(
		Platforms_Number : Positive;
		Name : String) return Station_Ref
	is
		Station : access Regional_Station_Type := new Regional_Station_Type(Platforms_Number);
	begin
		Station.Name := Unbounded_Strings.To_Unbounded_String(Name);
		for I in Positive range 1..Platforms_Number loop
			Station.Platforms(I) := new Platform.Platform_Type(I);
		end loop;
		Station.Panel := new Notice_Panel.Notice_Panel_Entity(1);
		return Station;
	end;

	procedure Print(This : Regional_Station_Type) is
	begin
		Put_Line ("Name : " & Unbounded_Strings.To_String(This.Name));
		Put_Line ("Platform Number : " & Integer'Image(This.Platforms_Number));
    end Print;

-------------------------------------- JSON - Regional Station --------------------------------------

	function Get_Regional_Station(Json_Station : Json_Value) return Station_Ref
	is
		Platforms_Number : Positive := Json_Station.Get("platform_number");
		Name : String				:= Json_Station.Get("name");
	begin
		return New_Regional_Station(Platforms_Number,Name);
	end Get_Regional_Station;

	-- #
	-- # Creates a Station_Array object containing the station defined in the given Json_Value
	-- #
	-- # @return A reference to the created Array
	-- #
	function Get_Regional_Station_Array(Json_v : Json_Value) return Stations_Array_Ref is
		J_Array : JSON_Array := Json_v.Get(Field => "stations");
		Array_Length : constant Natural := Length (J_Array);
		T : Stations_Array_Ref := new Stations_Array(1 .. Array_Length);
	begin

		for I in 1 .. T'Length loop
			T(I) := Get_Regional_Station(Get(Arr => J_Array, Index => I));
		end loop;

		return T;
	end Get_Regional_Station_Array;


	function Get_Regional_Station_Array(Json_Station : String) return Stations_Array_Ref is
	begin
		return Get_Regional_Station_Array(Get_Json_Value(Json_Station));
    end Get_Regional_Station_Array;


    overriding procedure Finalize   (This: in out Regional_Station_Type) is
    begin
    	Put_Line("finalize");
    end Finalize;

end Regional_Station;
