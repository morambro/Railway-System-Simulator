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
package body Ticket is

	function Get_Ticket(Json_String : String) return access Ticket_Type is
		-- Create JSON_Value object from the given json string
		Json_v : JSON_Value := Get_Json_Value(Json_String => Json_String);
		-- Extract "ticket" json array in J_Array variable
		J_Array : JSON_Array := Json_v.Get(Field => "ticket");
		-- Extract J_Array length
		Array_Length : constant Natural := Length (J_Array);
		-- Instantiate a new Ticket_Type with Array_Length elements
		T : access Ticket_Type := new Ticket_Type(1 .. Array_Length);

	begin
		-- For each element of the json array, create a new Ticket stage
		for I in 1 .. T'Length loop
			declare
				Json_Ticket : Json_Value := Get(Arr => J_Array, Index => I);
			begin
				T(I) := (
					Next_Station 	=> Json_Ticket.Get("next_station"),
					Train_ID 		=> Json_Ticket.Get("train_id"),
					Platform_Index	=> Json_Ticket.Get("platform_index")
				);
			end;
		end loop;

		return T;
	end Get_Ticket;

end Ticket;