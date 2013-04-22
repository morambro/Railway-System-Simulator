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
with Environment;
with Ada.Text_IO;
with YAMI.Parameters;
with Message_Agent;

package body Central_Office_Interface is

	procedure Ask_For_Ticket(
			From			: String;
			To				: String;
			Traveler_Index	: Positive) is
	begin
		-- # The resolution of the Ticket is not local, so perform a remote request.
		declare
			Parameters : YAMI.Parameters.Parameters_Collection := YAMI.Parameters.Make_Parameters;
		begin

			Parameters.Set_String("from",From);
			Parameters.Set_String("to",To);
			Parameters.Set_String("start_node",Environment.Get_Node_Name);
			Parameters.Set_String("traveler_index",Integer'Image(Traveler_Index));

			Message_Agent.Instance.Send(
				Destination_Address => Environment.Get_Central_Ticket_Office,
				Object 				=> "central_ticket_server",
				Service 			=> "resolve",
				Params 				=> Parameters,
				Callback			=> null
			);
		end;
    end Ask_For_Ticket;


end Central_Office_Interface;