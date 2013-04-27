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
with Ada.Text_IO;use Ada.Text_IO;
with Ada.Calendar.Formatting;

package body Time_Table is


	procedure Print(T : Time_Table_Array) is
	begin
		for I in 1 .. T'Length loop
			Put_Line ("Route = " & Integer'Image(T(I).Route_Index));
			Put_Line ("Table:");
			for J in 1 .. T(I).Table'Length loop
				for K in 1 .. T(I).Table(J)'Length loop
					declare
					begin
--  						Split (T(I).Table(J)(K),S,TS);
						-- Seconds_Count'Image(S)
						Put( Ada.Calendar.Formatting.Image(T(I).Table(J)(K))  & " , ");
					end;
				end loop;
				Put_Line("");
			end loop;
		end loop;
    end Print;


	function Get_Time_Table(Json_v : JSON_Value) return access Time_Table_Type
	is
		Route_Index		: Positive := Json_v.Get(Field => "route");
		Restart_Span    : Positive := Json_v.Get(Field => "restart_span");
		-- # Array of time array
		J_Array 		: JSON_Array := Json_v.Get(Field => "time");
		Array_Length 	: constant Natural := Length (J_Array);
		T_Table 		: access Time_Table_Type := new Time_Table_Type(Array_Length);
		Ref_Clock 		: Time := Clock;
	begin
		T_Table.Route_Index := Route_Index;
		T_Table.Restart_Span := Restart_Span;
		for I in 1 .. Array_Length loop
			declare
				Row		: JSON_Value := Get(Arr => J_Array,Index => I);
				T_Array : JSON_Array := Get(Row);
				T_Length  : constant Natural := Length (T_Array);
			begin
				T_Table.Table(I) := new Time_Array(1 .. T_Length);
				for J in 1 .. Length (T_Array) loop
					declare
						Span : Integer := Get(Get(Arr => T_Array,Index => J));
					begin
						T_Table.Table(I)(J) := Ref_Clock + Duration(Span);
					end;
				end loop;
			end;
		end loop;
		return T_Table;
    end Get_Time_Table;

	function Get_Time_Table_Array(Json_File : String) return Time_Table_Array
	is
		Json_v 			: JSON_Value := JSON_Helper.Get_Json_Value(Json_File_Name => Json_File);
		J_Array 		: JSON_Array := Json_v.Get(Field => "time_table");
		Array_Length 	: constant Natural := Length (J_Array);
		T_T_Array		: Time_Table_Array(1 .. Array_Length);
	begin
		for I in 1 .. Array_Length loop
			T_T_Array(I) := Get_Time_Table(Get(Arr => J_Array,Index => I));
		end loop;
		return T_T_Array;
    end Get_Time_Table_Array;



    function Get_Time_Representation(T : Time) return String is
    begin
		return Ada.Calendar.Formatting.Image(T);
    end Get_Time_Representation;



    procedure Update_Time_Table(
		This 	: access Time_Table_Type) is
	begin
		-- # If the current position on the array of times is
		if This.Current_Array_Position + 1 > This.Table(This.Current_Array_Index)'Length then
			This.Current_Array_Position := 1;
			if This.Current_Array_Index + 1 > This.Table'Length then

				for I in 1 .. This.Table'Length loop
					for J in 1 .. This.Table(I)'Length loop
						This.Table(I)(J) := This.Table(I)(J) + Duration(This.Restart_Span);
					end loop;
				end loop;

				This.Current_Array_Index := 1;
			else
				This.Current_Array_Index := This.Current_Array_Index + 1;
			end if;
		else
			This.Current_Array_Position :=  This.Current_Array_Position + 1;
		end if;



    end Update_Time_Table;


end Time_Table;