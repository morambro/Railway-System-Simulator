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
with JSON_Helper;use JSON_Helper;

package body Ticket is


	procedure Print(
		The_Ticket : Ticket_Type_Ref) is
	begin
		if The_Ticket = null then
			Ada.Text_IO.Put_Line("Null Ticket!");
			return;
		end if;
		Ada.Text_IO.Put_Line("Ticket Print : ");
		for I in 1 .. The_Ticket.Stages'Length loop
			Ada.Text_IO.Put_Line("Stage " & Integer'Image(I));
			Ada.Text_IO.Put_Line("    Start_Station : " & Integer'Image(The_Ticket.Stages(I).Start_Station));
			Ada.Text_IO.Put_Line("    Next_Station : " & Integer'Image(The_Ticket.Stages(I).Next_Station));
			Ada.Text_IO.Put_Line("    Region : " & To_String(The_Ticket.Stages(I).Region));
			Ada.Text_IO.Put_Line("    Train_ID : " & Integer'Image(The_Ticket.Stages(I).Train_ID));
			Ada.Text_IO.Put_Line("    Start_Platform_Index : " & Integer'Image(The_Ticket.Stages(I).Start_Platform_Index));
			Ada.Text_IO.Put_Line("    Destination_Platform_Index : " & Integer'Image(The_Ticket.Stages(I).Destination_Platform_Index));
			Ada.Text_IO.Put_Line("    Current_Run : " & Integer'Image(The_Ticket.Stages(I).Current_Run_Id));
		end loop;
    end Print;


	function Get_Ticket(Json_V : in JSON_Value) return Ticket_Type_Ref is
		-- Extract "ticket" json array in J_Array variable
		J_Array : JSON_Array := Json_V.Get(Field => "ticket");
--  		-- Extract J_Array length
		Array_Length : constant Natural := Length (J_Array);
--  		-- Instantiate a new Ticket_Type with Array_Length elements
		T : Ticket_Type_Ref := new Ticket_Type;

	begin
		T.Next_Stage 	:= Json_v.Get("next");
		T.Stages 		:= new Ticket_Stages(1..Array_Length);
		-- For each element of the json array, create a new Ticket stage
		for I in 1 .. T.Stages'Length loop
			declare
				Json_Ticket : Json_Value := Get(Arr => J_Array, Index => I);
			begin
				T.Stages(I) := (
					Start_Station				=> Json_Ticket.Get("start_station"),
					Next_Station 				=> Json_Ticket.Get("next_station"),
					Region						=> Json_Ticket.Get("region"),
					Train_ID 					=> Json_Ticket.Get("train_id"),
					Start_Platform_Index		=> Json_Ticket.Get("start_platform_index"),
					Current_Run_Id				=> Json_Ticket.Get("current_run"),
					Destination_Platform_Index	=> Json_Ticket.Get("destination_platform_index"));
			end;
		end loop;

		return T;
	end Get_Ticket;

	function Get_Ticket(Json_String : in String) return Ticket_Type_Ref is
	begin
		return Get_Ticket(Get_Json_Value(Json_String => Json_String));
    end Get_Ticket;


--      function Get_All_Tickets(Json_File : String) return access Tickets_Array is
--  		Json_v 			: JSON_Value := Get_Json_Value(Json_File_Name => Json_File);
--  		J_Array			: JSON_Array := Json_v.Get(Field => "tickets");
--  		Array_Length 	: constant Natural := Length (J_Array);
--  		To_Return 		: access Ticket.Tickets_Array := new Ticket.Tickets_Array(1..Array_Length);
--  	begin
--  		for I in 1..Array_Length loop
--  			To_Return(I) := Get_Ticket(Get(Arr => J_Array, Index => I));
--  		end loop;
--
--
--  		return To_Return;
--      end Get_All_Tickets;


    function To_Json(
		Ticket 		: Ticket_Type_Ref) return String
	is
		Json_Ticket : JSON_Value := Create_Object;
		Json_Fields : JSON_Array := Empty_Array;
	begin
		Json_Ticket.Set_Field("next",Ticket.Next_Stage);

		for I in 1 .. Ticket.Stages'Length loop
			declare
				JSon_Stage : JSON_Value := Create_Object;

			begin
				JSon_Stage.Set_Field("start_station",Ticket.Stages(I).Start_Station);
				JSon_Stage.Set_Field("next_station",Ticket.Stages(I).Next_Station);
				JSon_Stage.Set_Field("train_id",Ticket.Stages(I).Train_ID);
				JSon_Stage.Set_Field("region",Ticket.Stages(I).Region);
				JSon_Stage.Set_Field("start_platform_index",Ticket.Stages(I).Start_Platform_Index);
				JSon_Stage.Set_Field("destination_platform_index",Ticket.Stages(I).Destination_Platform_Index);

				Append(Json_Fields,JSon_Stage);
			end;
		end loop;

		Json_Ticket.Set_Field("ticket",Json_Fields);
		return Json_Ticket.Write;
	end To_Json;


end Ticket;