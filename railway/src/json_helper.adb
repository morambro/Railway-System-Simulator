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
--  Railway_Simulation is distributed in the hope that it will be useful,			--
--  but WITHOUT ANY WARRANTY; without even the implied warranty of				--
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the				--
--  GNU General Public License for more details.								--
--																				--
--  You should have received a copy of the GNU General Public License			--
--  along with Railway_Simulation.  If not, see <http://www.gnu.org/licenses/>.		--
----------------------------------------------------------------------------------

with Ada.Direct_IO;
with Ada.Directories;
with Ada.Text_IO;use Ada.Text_IO;

package body JSON_Helper is

	function Load_File(File_Name : String) return String is

		use Ada.Directories;

		File_Size    : constant Natural := Natural (Size (File_Name));

		subtype Test_JSON_Str is String (1 .. File_Size);
		package File_IO is new Ada.Direct_IO (Test_JSON_Str);

		File           : File_IO.File_Type;
		String_Content : Test_JSON_Str;

	begin
		File_IO.Open (File => File,
		       Mode => File_IO.In_File,
		       Name => File_Name);
		File_IO.Read (File => File,
		       Item => String_Content);
		File_IO.Close (File => File);

		return String_Content;

    end Load_File;

    function Get_Json_Value(Json_String : String := ""; Json_File_Name : String := "") return JSON_Value is
    	Json : JSON_Value;
    begin
		if Json_String = "" then
			Json := Read (
				Strm 		=> Load_File (Json_File_Name),
	            Filename 	=> "");
	    elsif Json_File_Name = "" then
			Json := Read (
				Strm 		=> Json_String,
	            Filename 	=> "");
	    end if;

		return Json;

    end Get_Json_Value;

   	procedure Handler (
   		Name  : in UTF8_String;
      	Value : in JSON_Value)
    is
	use Ada.Text_IO;
    begin
		case Kind (Val => Value) is
		    when JSON_Null_Type =>
				Put_Line (Name & "(null):null");
		    when JSON_Boolean_Type =>
				Put_Line (Name & "(boolean):" & Boolean'Image (Get (Value)));
		    when JSON_Int_Type =>
				Put_Line (Name & "(integer):" & Integer'Image (Get (Value)));
		    when JSON_Float_Type =>
				Put_Line (Name & "(float):" & Float'Image (Get (Value)));
		    when JSON_String_Type =>
				Put_Line (Name & "(string):" & Get (Value));
		    when JSON_Array_Type =>
				declare
				    A_JSON_Array : constant JSON_Array := Get (Val => Value);
				    A_JSON_Value : JSON_Value;
				    Array_Length : constant Natural := Length (A_JSON_Array);
				begin
				    Put (Name & "(array):[");
				    for J in 1 .. Array_Length loop
						A_JSON_Value := Get (Arr   => A_JSON_Array,
						Index => J);
						Put (Get (A_JSON_Value));

						if J < Array_Length then
						    Put (", ");
						end if;
				    end loop;
				    Put ("]");
				    New_Line;
				end;
		    when JSON_Object_Type =>
				Put_Line (Name & "(object):");
				Map_JSON_Object (Val => Value,CB  => Handler'Access);
		end case;
	--  Decide output depending on the kind of JSON field we're dealing with.
	--  Note that if we get a JSON_Object_Type, then we recursively call
	--  Map_JSON_Object again, which in turn calls this Handler procedure.
    end Handler;

    procedure Print_Json(Text : String) is
    begin
		Map_JSON_Object (
			Val   => Get_Json_Value(Text),
            CB    => Handler'Access);
    end Print_Json;

	procedure Print_Json_value(J : Json_Value) is
	begin
		Map_JSON_Object (
			Val   => J,
            CB    => Handler'Access);
    end Print_Json_value;

    function "<" (a,b : SU.Unbounded_String) return Boolean is
    begin
		return SU.To_String(a) < SU.To_String(b);
    end "<";


	function "=" (a,b : SU.Unbounded_String) return Boolean is
	begin
		return SU.To_String(a) = SU.To_String(b);
    end "=";

end Json_Helper;
