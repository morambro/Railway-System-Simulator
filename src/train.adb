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