----------------------------------------------------------------------------------
--  Copyright 2013                                								--
--  Moreno Ambrosin                         									--
--  Railway_Simulation 1.0                                       				--
--  Concurrent and Distributed Systems class project  							--
--  Master Degree in Computer Science                 							--
--  Academic year 2012/2013                              						--
--  Dept. of Pure and Applied Mathematics             							--
--  University of Padua, Italy                        							--
--                                                    							--
--  This file is part of Railway_Simulation project.							--
--																				--
--  Railway_Simulation is free software: you can redistribute it and/or modify	--
--  it under the terms of the GNU General Public License as published by		--
--  the Free Software Foundation, either version 3 of the License, or			--
--  (at your option) any later version.											--
--																				--
--  Railway_Simulation is distributed in the hope that it will be useful,		--
--  but WITHOUT ANY WARRANTY; without even the implied warranty of				--
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the				--
--  GNU General Public License for more details.								--
--																				--
--  You should have received a copy of the GNU General Public License			--
--  along with Railway_Simulation.  If not, see <http://www.gnu.org/licenses/>. --
----------------------------------------------------------------------------------
with Ada.Text_IO;

package body Train is


    function String_To_Train_Type (
		T_Type			: in String) return Train_Type is
	begin
		if T_Type = "FB" or T_Type = "fb" then
			return FB;
		else
			return REGIONAL;
		end if;
	end String_To_Train_Type;

	function Train_Type_To_String(
		T_Type	: in Train_Type) return String is
	begin
		if T_Type = FB then
			return "fb";
		end if;
		return "regional";

	end Train_Type_To_String;

	function Get_Trains_Array(Json_File_Name : String) return Trains_Array is
		Json_v  : Json_Value := Get_Json_Value(Json_File_Name => Json_File_Name);
		J_Array : constant JSON_Array := Json_v.Get(Field => "trains");
		Array_Length : constant Natural := Length (J_Array);
		T : Trains_Array(1 .. Array_Length);
	begin
		for I in 1 .. T'Length loop
			T(I) := Get_Train_Descriptor(Get(Arr => J_Array, Index => I));
		end loop;
		return T;
	end Get_Trains_Array;

	function Get_Train_Descriptor(Json_Train : Json_Value) return Train_Descriptor is

		Strat_Node : String := Json_Train.Get("start_node");

		To_Return : Train_Descriptor := (
			Id 				=> Json_Train.Get("id"),
			Speed 			=> Json_Train.Get("speed"),
		    Max_Speed 		=> Json_Train.Get("max_speed"),
		    Current_Station => Json_Train.Get("current_station"),
		    Next_Stage		=> Json_Train.Get("next_stage"),
		    Route_Index		=> Json_Train.Get("route_index"),
		    Sits_Number 	=> Json_Train.Get("sits_number"),
		    Occupied_Sits	=> Json_Train.Get("occupied_sits"),
		    T_Type			=> String_To_Train_Type(Json_Train.Get("type")),
		    Start_Node 		=> To_Unbounded_String(Strat_Node)
		);
	begin
		return To_Return;
    end Get_Train_Descriptor;


	function Get_Train_Descriptor (
		Json_Train 		: in String) return Train_Descriptor is
	begin
		return Get_Train_Descriptor(Get_Json_Value(Json_Train));
    end Get_Train_Descriptor;


	function Get_Json(
		Train 	: Train_Descriptor) return String
	is
		Json_Train : JSON_Value := Create_Object;
	begin

		Json_Train.Set_Field("id",Train.Id);
		Json_Train.Set_Field("speed",Train.Speed);
		Json_Train.Set_Field("max_speed",Train.Max_Speed);
		Json_Train.Set_Field("current_station",Train.Current_Station);
		Json_Train.Set_Field("next_stage",Train.Next_Stage);
		Json_Train.Set_Field("route_index",Train.Route_Index);
		Json_Train.Set_Field("sits_number",Train.Sits_Number);
		Json_Train.Set_Field("occupied_sits",Train.Occupied_Sits);
		Json_Train.Set_Field("type",Train_Type_To_String(Train.T_Type));


		return Json_Train.Write;
	end;


	procedure Print(T : Train_Descriptor) is
	begin
		Ada.Text_IO.Put_Line("");
		Ada.Text_IO.Put_Line("Train Descriptor:");
		Ada.Text_IO.Put_Line("   Id : " & Integer'Image(T.Id));
		Ada.Text_IO.Put_Line("   Speed : " & Integer'Image(T.Speed));
		Ada.Text_IO.Put_Line("   Max_Speed : " & Integer'Image(T.Max_Speed));
		Ada.Text_IO.Put_Line("   Current_Station : " & Integer'Image(T.Current_Station));
		Ada.Text_IO.Put_Line("   Next_Stage : " & Integer'Image(T.Next_Stage));
		Ada.Text_IO.Put_Line("   Route_Index : " & Integer'Image(T.Route_Index));
		Ada.Text_IO.Put_Line("   Occupied_Sits : " & Integer'Image(T.Occupied_Sits));
		if T.T_Type = FB then
			Ada.Text_IO.Put_Line("   Type : FB");
		else
			Ada.Text_IO.Put_Line("   Type : Regional");
		end if;
		Ada.Text_IO.Put_Line("");
    end Print;

end Train;