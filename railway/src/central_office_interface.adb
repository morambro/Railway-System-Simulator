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
				Callback 			=> null);
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
				declare
					-- # Extract the validated ticket returned by the Central Office
					Validated_Ticket : Ticket_Type_Ref := Ticket.Get_Ticket(Content.Get_String("ticket"));
					Old_Ticket 		 : Ticket_Type_Ref := The_Ticket;
				begin
					-- # Delete the Old non-validated ticket
					Free_Ticket(Old_Ticket);
					-- # Call the callback procedure with the new validated ticket.
					Callback(Validated_Ticket,True);
				end;
			elsif Response = "FALSE" then
				-- # Callback procedure communicating the result
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
				Callback			=> Process_Result'Access);

		end;
    end Validate;


	procedure Update_Run(
		Route_Index 	: Positive;
		Current_Run		: Positive;
		Callback 		: access procedure(
			Updated			: in 	 Boolean;
			New_Time_Table 	: access Time_Table.Time_Table_Type;
			Current_Run_Id	: in	 Natural))
	is
		Parameters : YAMI.Parameters.Parameters_Collection := YAMI.Parameters.Make_Parameters;

		-- #
		-- # Callback procedure used to collect the data retrieved by the Central Ticket Office, and
		-- # call the callback procedure passed as a parameter.
		-- #
		procedure Process_Result(Content : in out YAMI.Parameters.Parameters_Collection)
		is
			Response : String := Content.Get_String("response");
		begin
			if Response = "UPDATED" then
				declare
					Time_T : String := Content.Get_String("new_time_table");
				begin
					-- # Call the Callback procedure to update the data.
					Callback(
						True,
						Time_Table.Get_Time_Table(Time_T),0);
				end;
			elsif Response = "OK" then
				-- # Do nothing, simply log it
				Logger.Log(
					"Central_Ticket_Office",
					"Current_Run updated.",
					Logger.DEBUG);
				declare
					Current_Run_Id : Natural := Integer(Content.Get_Integer("run_id"));
				begin
					Callback(
						False,
						null,
						Current_Run_Id);
				end;
			end if;
		end Process_Result;

	begin
		Parameters.Set_Integer("route_index",YAMI.Parameters.YAMI_Integer(Route_Index));
		Parameters.Set_Integer("current_run",YAMI.Parameters.YAMI_Integer(Current_Run));

		Message_Agent.Instance.Send(
			Destination_Address => Environment.Get_Central_Ticket_Office,
			Object 				=> "central_ticket_server",
			Service 			=> "update_run",
			Params 				=> Parameters,
			Callback			=> Process_Result'Access);

    end Update_Run;


	procedure Load_Time_Tables(
			Callback 		: access procedure (Table : in	Time_Table.Time_Table_Array_Ref))
	is
		Parameters : YAMI.Parameters.Parameters_Collection := YAMI.Parameters.Make_Parameters;

		procedure Process_Result(Content : in out YAMI.Parameters.Parameters_Collection)
		is
			Time_Tables : String := Content.Get_String("time_tables");
		begin
			-- # Once all the time tables have been retrieved, give them to the callback procedure.
			Callback(Time_Table.Get_Time_Table_Array(Time_Tables));
		end Process_Result;
	begin
		-- # Send the request without parameters
		Message_Agent.Instance.Send(
			Destination_Address => Environment.Get_Central_Ticket_Office,
			Object 				=> "central_ticket_server",
			Service 			=> "get_time_table",
			Params 				=> Parameters,
			Callback			=> Process_Result'Access);

    end Load_Time_Tables;

end Central_Office_Interface;