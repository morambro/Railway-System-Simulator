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
with YAMI.Parameters;
with Message_Agent;
with Ada.Exceptions;
with Logger;

package body Name_Server_Interface is

	procedure Bind (
			Name_Server : access String;
			Node_Name 	: access String;
			Address		: access String)
	is
		Params : YAMI.Parameters.Parameters_Collection := YAMI.Parameters.Make_Parameters;
	begin
		Params.Set_String("node_name",Node_Name.all);
		Params.Set_String("address",Address.all);

		Message_Agent.Instance.Send(
			Destination_Address => Name_Server.all,
			Object 				=> "name_server",
			Service 			=> "add",
			Params 				=> Params,
			Callback			=> null
		);
	exception
		when E : YAMI.Runtime_Error => raise Name_Server_Exception with Ada.Exceptions.Exception_Message(E);
    end Bind;

    procedure Resolve (
		Name_Server : in	 String;
		Node_Name 	: in	 String;
		Callback	: access procedure(Response : String))
	is
		Parameters : YAMI.Parameters.Parameters_Collection := YAMI.Parameters.Make_Parameters;

		-- #
		-- # Callback procedure used to collect the data retrieved by the Name Server, and
		-- # call the callback procedure passed as a parameter.
		-- #
		procedure Process_Result(Content : in out YAMI.Parameters.Parameters_Collection)
		is
			Address : String := Content.Get_String("response");
		begin
			if Address /= "_" then
				-- # Add the found Address to the List
				Last_Addresses.Insert(Node_Name,Address);
				Callback(Address);
			else
				Logger.Log(
					Sender 		=> "Name_Server_Interface.Resolve",
					Message		=> "Unable to locate " & Node_Name,
					L			=> Logger.ERROR);
			end if;
		end Process_Result;

	begin
		if Last_Addresses.Contains(Node_Name) then
			-- # If the searched information is local, return immediately
			Callback(Last_Addresses.Element(Node_Name));
		else
			Parameters.Set_String("node_name",Node_Name);

			Message_Agent.Instance.Send(
				Destination_Address => Name_Server,
				Object 				=> "name_server",
				Service 			=> "get",
				Params 				=> Parameters,
				Callback			=> Process_Result'Access
			);
		end if;
    end Resolve;


end Name_Server_Interface;