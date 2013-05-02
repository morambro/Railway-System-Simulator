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
with Ada.Real_Time;
with Central_Office_Interface;

package body Time_Table is

	procedure Print(T : Time_Table_Array_Ref) is
	begin
		for I in 1 .. T'Length loop
			Put_Line ("Route = " & Integer'Image(T(I).Route_Index));
			Put_Line ("Table:");
			for J in 1 .. T(I).Table'Length loop
				for K in 1 .. T(I).Table(J)'Length loop
					declare
					begin
						Put( Ada.Calendar.Formatting.Image(T(I).Table(J)(K))  & " , ");
					end;
				end loop;
				Put_Line("");
			end loop;
		end loop;
    end Print;


	function Get_Time_Table(Json_v : JSON_Value) return access Time_Table_Type
	is
		Route_Index		: Positive := Json_v.Get(Field => "route_index");
		Current_Run    	: Positive := Json_v.Get(Field => "current_run");
		Current_Run_Id 	: Positive := Json_v.Get(Field => "current_run_id");
		-- # Array of time array
		J_Array 		: JSON_Array := Json_v.Get(Field => "time");
		Array_Length 	: constant Natural := Length (J_Array);
		T_Table 		: access Time_Table_Type := new Time_Table_Type(Array_Length);
		Ref_Clock 		: Time := Clock;
	begin
		T_Table.Route_Index 	:= Route_Index;
		T_Table.Current_Run 	:= Current_Run;
		T_Table.Current_Run_Id 	:= Current_Run_Id;
		for I in 1 .. Array_Length loop
			declare
				Row		: JSON_Value := Get(Arr => J_Array,Index => I);
				T_Array : JSON_Array := Get(Row);
				T_Length  : constant Natural := Length (T_Array);
			begin
				T_Table.Table(I) := new Time_Array(1 .. T_Length);
				for J in 1 .. Length (T_Array) loop
					declare
						-- # Retrieve the String representation of the Date in the format "YYYY-MM-dd HH:mm:ss"
						Date : String := Get(Get(Arr => T_Array,Index => J));
					begin
						-- # Create a Time object from String value.
						T_Table.Table(I)(J) := Ada.Calendar.Formatting.Value(Date);--Ref_Clock + Duration(Span);
					end;
				end loop;
			end;
		end loop;

		return T_Table;
    end Get_Time_Table;


    function Get_Time_Table(Json_v : String) return access Time_Table_Type is
	begin
		return Get_Time_Table(JSON_Helper.Get_Json_Value(Json_String => Json_v));
    end Get_Time_Table;

	function Get_Time_Table_Array(Json_File : String) return Time_Table_Array_Ref
	is
		Json_v 			: JSON_Value := JSON_Helper.Get_Json_Value(Json_String => Json_File);
		J_Array 		: JSON_Array := Json_v.Get(Field => "time_table");
		Array_Length 	: constant Natural := Length (J_Array);
		T_T_Array		: Time_Table_Array_Ref := new Time_Table_Array(1 .. Array_Length);
	begin
		for I in 1 .. Array_Length loop
			T_T_Array(I) := Get_Time_Table(Get(Arr => J_Array,Index => I));
		end loop;

		Print(T_T_Array);

		return T_T_Array;
    end Get_Time_Table_Array;



    function Get_Time_Representation(T : Time) return String is
    begin
		return Ada.Calendar.Formatting.Image(T);
    end Get_Time_Representation;



    procedure Update_Time_Table(
		This 	: access Time_Table_Type)
	is
		-- #
		-- # Callback procedure used to update time table in case the request
		-- # modifies it.
		-- #
		procedure Callback(
			Updated			: in 	 Boolean;
			New_Time_Table 	: access Time_Table_Type;
			Current_Run_Id	: in	 Natural) is
		begin
			if Updated then
				-- # Current_Run now is the first one
				This.Current_Run 	:= 1;
				-- # Update Current_Run_Id
				This.Current_Run_Id := New_Time_Table.Current_Run_Id;

				-- # Set the new Time Table.
				This.Table 			:= New_Time_Table.Table;
			else
				This.Current_Run_Id := Current_Run_Id;
			end if;

		end Callback;

	begin
		-- # If the current position on the array of times is
		if This.Current_Run_Cursor + 1 > This.Table(This.Current_Run)'Length then

			This.Current_Run_Cursor := 1;
			-- # Increase by one the Current_Run value, and communicate
			-- # this to the Central Ticket Office.
			This.Current_Run := This.Current_Run + 1;

			-- # If the Time Table is updated, Current_Run will be set to 1.
			Central_Office_Interface.Update_Run(
				Route_Index		=> This.Route_Index,
				Current_Run		=> This.Current_Run,
				Callback		=> Callback'Access);

		else
			-- # No run changes, so just increase the cursor by one.
			This.Current_Run_Cursor :=  This.Current_Run_Cursor + 1;
		end if;
    end Update_Time_Table;


end Time_Table;