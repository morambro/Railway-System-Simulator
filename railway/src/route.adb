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

with JSON_Helper;
with Ada.Text_IO;

package body Route is

	use JSON_Helper;

	function Get_Routes (Json_File : String) return Routes is
		Json_v 			: JSON_Value := Get_Json_Value(Json_File_Name => Json_File);
		J_Array 		: JSON_Array := Json_v.Get(Field => "routes");
		Array_Length 	: constant Natural := Length (J_Array);
		All_Routes	  	: Routes(1 .. Array_Length);
	begin
		for I in 1 .. Array_Length loop
			All_Routes(I) := Get_Route (Get(Arr => J_Array,Index => I));
		end loop;
		return All_Routes;
    end Get_Routes;


	function StringToAction(Act : String) return Action is
	begin
		if Act = "PASS" then
			return PASS;
		end if;
		return ENTER;
    end Stringtoaction;


	function Get_Route (Json_v : JSON_Value) return access Route_Type is
		J_Array 		: JSON_Array := Json_v.Get(Field => "route");
		Array_Length 	: constant Natural := Length (J_Array);
		Route		  	: access Route_Type := new Route_Type (1 .. Array_Length);
	begin

		for I in 1 .. Array_length loop
			declare
				Json_Stage 	: JSON_Value := Get(Arr => J_Array,Index => I);
			begin
				Route(I) := (
					Next_Segment 		=> Json_Stage.Get("next_segment"),
					Next_Station 		=> Json_Stage.Get("next_station"),
					Platform_Index 		=> Json_Stage.Get("platform_index"),
					Node_Name			=> Json_Stage.Get("node_name"),
					Enter_Action		=> StringToAction(Json_Stage.Get("enter_action")),
					Leave_Action		=> StringToAction(Json_Stage.Get("leave_action")),
					Start_Station 		=> Json_Stage.Get("start_station"),
					Start_Platform		=> Json_Stage.Get("start_platform")
				);
			end;
		end loop;
		return Route;
    end Get_Route;

    procedure Print(R : Route_Type) is
    begin
    	for I in 1 .. R'Length loop
    		Ada.Text_IO.Put_Line(
    			"Next Segment     : " & Integer'Image(R(I).Next_Segment) & ASCII.LF &
    			"Next Station     : " & Integer'Image(R(I).Next_Station) & ASCII.LF &
    			"Platform Index   : " & Integer'Image(R(I).Platform_Index) & ASCII.LF &
    			"Leave Action     : " & (if (R(I).Leave_Action = PASS) then "PASS" else "ENTER") & ASCII.LF &
    			"Enter Action     : " & (if (R(I).Enter_Action = PASS) then "PASS" else "ENTER") & ASCII.LF
    		);
    	end loop;
    end Print;

end Route;
