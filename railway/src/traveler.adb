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

package body Traveler is

	function Get_Name(Traveler : Traveler_Manager) return String is
	begin
		return To_String(Traveler.Traveler.Name);
	end Get_Name;

	procedure Print(T : Traveler_Manager) is
	begin
		Ada.Text_IO.Put_Line("ID : " & Integer'Image(T.Traveler.ID));
		Ada.Text_IO.Put_Line("Name : " & To_String(T.Traveler.Name));
		Ada.Text_IO.Put_Line("Surname : " & To_String(T.Traveler.Surname));
		Ada.Text_IO.Put_Line("Destination : " & To_String(T.Destination));
    end Print;

---------------------------------------- JSON-Traveler Creation ----------------------------------------------

	function Get_Traveler_Manager(Json_Traveler : String) return Traveler_Manager is
	begin
		return Get_Traveler_Manager(Get_Json_Value(Json_Traveler));
    end Get_Traveler_Manager;

    function Get_Traveler(Json_Traveler : JSON_Value) return Traveler_Type is
    	T : Traveler_Type;
    begin
		T.ID 			:= Json_Traveler.Get("id");
		T.Name 			:= Json_Traveler.Get("name");
		T.Surname 		:= Json_Traveler.Get("surname");
		return T;
    end;

    function Get_Traveler_Manager(Json_Traveler : JSON_Value) return Traveler_Manager is
    	T : Traveler_Manager;
    begin
    	T.Traveler		 := Get_Traveler(Json_Traveler.Get("traveler"));

		declare
			OP : Integer := Json_Traveler.Get("next_operation");
		begin
			case OP is
				when 1 => T.Next_Operation := LEAVE;
				when 2 => T.Next_Operation := ENTER;
				-- TODO: TO FIX!!!!
				when others => T.Next_Operation := LEAVE;
			end case;
		end;
		declare
			Dest : String := Json_Traveler.Get("destination");
		begin
			T.Destination := To_Unbounded_String(Dest);
		end;

		T.Start_Station	:= Json_Traveler.Get("start_station");

		return T;
    end Get_Traveler_Manager;

    function Get_Traveler_Manager_Array(Json_Traveler : String) return Traveler_Manager_Array_Ref is
		Json_v : JSON_Value := Get_Json_Value(Json_File_Name => Json_Traveler);
    	A_JSON_Array : constant JSON_Array := Get (Val => Json_v,Field => "travelers");
	    A_JSON_Value : JSON_Value;
	    Array_Length : constant Natural := Length (A_JSON_Array);
		T : Traveler_Manager_Array_Ref := new Traveler_Manager_Array(1 .. Array_Length);

    begin
    	for J in 1 .. Array_Length loop
			A_JSON_Value := Get (Arr => A_JSON_Array,Index => J);
			T(J) := Get_Traveler_Manager(A_JSON_Value);
	    end loop;
		return T;
    end Get_Traveler_Manager_Array;


	function Get_Json(
		Traveler : Traveler_Manager) return String
	is
		Json_Traveler_M : JSON_Value := Create_Object;
		Json_Traveler 	: JSON_Value := Create_Object;
	begin

		Json_Traveler.Set_Field("id",Traveler.Traveler.ID);
		Json_Traveler.Set_Field("name",Traveler.Traveler.Name);
		Json_Traveler.Set_Field("surname",Traveler.Traveler.Surname);

		Json_Traveler_M.Set_Field("traveler",Json_Traveler);
		Json_Traveler_M.Set_Field("destination",Traveler.Destination);

		case Traveler.Next_Operation is
			when BUY_TICKET 	=> Json_Traveler_M.Set_Field("next_operation","buy_ticket");
			when LEAVE			=> Json_Traveler_M.Set_Field("next_operation","leave");
			when ENTER			=> Json_Traveler_M.Set_Field("next_operation","enter");
			when TICKET_READY 	=> Json_Traveler_M.Set_Field("next_operation","ticket_ready");
		end case;



		return Json_Traveler_M.Write;
    end Get_Json;

end Traveler;
