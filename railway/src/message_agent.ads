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

with YAMI.Outgoing_Messages; use YAMI.Outgoing_Messages;
with YAMI.Agents;use YAMI.Agents;
with YAMI.Parameters;use YAMI.Parameters;
with YAMI.Option_Names; use YAMI.Option_Names;
with Unchecked_Deallocation;
with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Strings.Hash;
with YAMI.Incoming_Messages;

package Message_Agent is

	type Handler is access procedure(Content : in out YAMI.Parameters.Parameters_Collection);

	package Maps is new Ada.Containers.Indefinite_Hashed_Maps(
		Key_Type 		=> String,
		Element_Type 	=> Handler,
		Hash			=> Ada.Strings.Hash,
		Equivalent_Keys => "="
	);

	use Maps;
	-- #######################################################################################################
	-- #
	-- # A message Agent, used to send remote messages.
	-- #
	type Message_Agent_Type(Options : Parameters_Collection_Access) is tagged limited private;

	-- #
	-- # The reference for the declared tagged type
	-- #
	type Message_Agent_Ref is access all Message_Agent_Type;

		-- #
		-- # This method is used to Send a remote message to a Destination.
		-- #
		procedure Send(
			This 				: access Message_Agent_Type;
			Destination_Address : in 	 String;
			Object 				: in 	 String;
			Service 			: in 	 String;
			Params 				: in 	 YAMI.Parameters.Parameters_Collection;
			-- # Procedure that will be called on destination's replace (if needed)
			Callback			: access procedure(Content : in out YAMI.Parameters.Parameters_Collection));

		-- #
		-- # Makes the Agent listen on a specific address.
		-- #
		procedure Listen_To(
			This 				: access Message_Agent_Type;
			Server_Address 		: in	 String);

		procedure Close(
			This				: access Message_Agent_Type);


		-- #
		-- # Adds an Handler, to manage messages received from the given Service
		-- #
		procedure Add_Handler(
			This				: access Message_Agent_Type;
			Service				: in String;
			The_Handler			: Handler);

	-- #######################################################################################################

	-- #
	-- # A Main Handler type used to dispatch the incoming messages
	-- #
	type Main_Message_Handler is new YAMI.Incoming_Messages.Message_Handler with record
		Agent 		: access Message_Agent_Type;
	end record;

		overriding procedure Call(
	    	This	: in	out Main_Message_Handler;
	  		Message : in	out YAMI.Incoming_Messages.Incoming_Message'Class);


	-- #######################################################################################################

	-- #
	-- # Default callback procedure.
	-- #
	procedure Process_Reply(Content : in out YAMI.Parameters.Parameters_Collection);

	procedure Init;

	-- #
	-- # The instance of Message_Agent.
	-- #
	Instance : Message_Agent_Ref := null;

private

	type Message_Agent_Type(Options : Parameters_Collection_Access) is tagged limited record
		Client_Agent 	: YAMI.Agents.Agent_Access := YAMI.Agents.New_Agent(Options.all);
		Handlers_Map	: Maps.Map;
	end record;

	Main_Handler 	 : aliased Main_Message_Handler;
end Message_Agent;
