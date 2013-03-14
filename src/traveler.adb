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
		return Unbounded_Strings.To_String(Traveler.Traveler.Name);
	end Get_Name;

	procedure Print(T : Traveler_Manager) is
	begin
		Ada.Text_IO.Put_Line("ID : " & Integer'Image(T.Traveler.ID));
		Ada.Text_IO.Put_Line("Name : " & Unbounded_Strings.To_String(T.Traveler.Name));
		Ada.Text_IO.Put_Line("Surname : " & Unbounded_Strings.To_String(T.Traveler.Surname));
		Ada.Text_IO.Put_Line("Next Op : " & Integer'Image(T.Next_Operation));
		Ada.Text_IO.Put_Line("Destination : " & Integer'Image(T.Destination));
    end Print;

---------------------------------------- JSON-Traveler Creation ----------------------------------------------

    function Get_Traveler(Json_Traveler : JSON_Value) return Traveler_Type is
    	T : Traveler_Type;
    begin
		T.ID 		:= Json_Traveler.Get("id");
		T.Name 		:= Json_Traveler.Get("name");
		T.Surname 	:= Json_Traveler.Get("surname");
		return T;
    end;

    function Get_Traveler_Manager(Json_Traveler : JSON_Value) return Traveler_Manager is
    	T : Traveler_Manager;
    begin
    	T.Traveler		 := Get_Traveler(Json_Traveler.Get("traveler"));
		T.Next_Operation := Json_Traveler.Get("next_operation");
		T.Destination	 := Json_Traveler.Get("destination");
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

end Traveler;
