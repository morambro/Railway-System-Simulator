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
with Logger;
with Ada.Calendar.Formatting;
with Ada.Calendar;

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
			-- # Add the time at witch the request is made
			Parameters.Set_String("request_time",Ada.Calendar.Formatting.Image(Ada.Calendar.Clock));

			Message_Agent.Instance.Send(
				Destination_Address => Environment.Get_Central_Ticket_Office,
				Object 				=> "central_ticket_server",
				Service 			=> "resolve",
				Params 				=> Parameters,
				Callback			=> null
			);
		end;
    end Ask_For_Ticket;

    procedure Validate(
			The_Ticket 	: Ticket.Ticket_Type_Ref;
			Callback 	: access procedure(The_Ticket:Ticket.Ticket_Type_Ref;Response: Boolean))
	is

		-- #
		-- # Callback procedure used to collect the data retrieved by the Central Ticket Office, and
		-- # call the callback procedure passed as a parameter.
		-- #
		procedure Process_Result(Content : in out YAMI.Parameters.Parameters_Collection)
		is
			Response : String := Content.Get_String("response");
		begin
			if Response = "TRUE" then
				Callback(The_Ticket,True);
			elsif Response = "FALSE" then
				Callback(The_Ticket,False);
			else
				Logger.Log(
					Sender 		=> "Central_Ticket_Office.Validate",
					Message		=> "Validation request Error",
					L			=> Logger.ERROR);
			end if;
		end Process_Result;

	begin
		if The_Ticket = null then
			Callback(The_Ticket,False);
			return;
		end if;
		-- # Not null Ticket, so let's send a validation request
		declare
			Parameters : YAMI.Parameters.Parameters_Collection := YAMI.Parameters.Make_Parameters;
		begin

			Parameters.Set_String("ticket",To_Json(The_Ticket));
			-- # Add the time at witch the request is made
			Parameters.Set_String("request_time",Ada.Calendar.Formatting.Image(Ada.Calendar.Clock));

			Message_Agent.Instance.Send(
				Destination_Address => Environment.Get_Central_Ticket_Office,
				Object 				=> "central_ticket_server",
				Service 			=> "validate",
				Params 				=> Parameters,
				Callback			=> Process_Result'Access
			);

		end;
    end Validate;


end Central_Office_Interface;