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

	function Get_Ticket(Json_V : in JSON_Value) return access Ticket_Type is
		-- Extract "ticket" json array in J_Array variable
		J_Array : JSON_Array := Json_V.Get(Field => "ticket");
--  		-- Extract J_Array length
		Array_Length : constant Natural := Length (J_Array);
--  		-- Instantiate a new Ticket_Type with Array_Length elements
		T : access Ticket_Type := new Ticket_Type(Array_Length);

	begin
		T.Next_Stage := Json_v.Get("next");
		-- For each element of the json array, create a new Ticket stage
		for I in 1 .. T.Stages'Length loop
			declare
				Json_Ticket : Json_Value := Get(Arr => J_Array, Index => I);
			begin
				T.Stages(I) := (
					Start_Station				=> Json_Ticket.Get("start_station"),
					Next_Station 				=> Json_Ticket.Get("next_station"),
					Train_ID 					=> Json_Ticket.Get("train_id"),
					Start_Platform_Index		=> Json_Ticket.Get("start_platform_index"),
					Destination_Platform_Index	=> Json_Ticket.Get("destination_platform_index")
				);
			end;
		end loop;

		return T;
	end Get_Ticket;

	function Get_Ticket(Json_String : in String) return access Ticket_Type is
	begin
		return Get_Ticket(Get_Json_Value(Json_String => Json_String));
    end Get_Ticket;


    function Get_All_Tickets(Json_File : String) return access Tickets_Array is
		Json_v 			: JSON_Value := Get_Json_Value(Json_File_Name => Json_File);
		J_Array			: JSON_Array := Json_v.Get(Field => "tickets");
		Array_Length 	: constant Natural := Length (J_Array);
		To_Return 		: access Ticket.Tickets_Array := new Ticket.Tickets_Array(1..Array_Length);
	begin
		for I in 1..Array_Length loop
			To_Return(I) := Get_Ticket(Get(Arr => J_Array, Index => I));
		end loop;


		return To_Return;
    end Get_All_Tickets;

end Ticket;