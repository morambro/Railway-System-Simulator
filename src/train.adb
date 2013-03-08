--==============================================================================
-- Created by:
--		Moreno Ambrosin
--  	Mat.  : 1035635
-- Date:
-- 		08/03/2013
--==============================================================================

package body Train is

	function Get_Trains_Array(Json_File_Name : String) return Trains_Array is
		Json_v  : Json_Value := Get_Json_Value(Json_File_Name);
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
		To_Return : Train_Descriptor := (
			Id 				=> Json_Train.Get("id"),
			Speed 			=> Json_Train.Get("speed"),
		    Max_Speed 		=> Json_Train.Get("max_speed"),
		    Current_Station => Json_Train.Get("current_station"),
		    Next_Stage		=> Json_Train.Get("next_stage"),
		    Route_Index		=> Json_Train.Get("route_index")
		);
	begin
		return To_Return;
    end Get_Train_Descriptor;

end Train;