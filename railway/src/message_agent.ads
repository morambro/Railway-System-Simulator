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

package Message_Agent is

	type Message_Agent_Type(Options : Parameters_Collection_Access) is tagged limited private;

	type Message_Agent_Ref is access all Message_Agent_Type;

	procedure Send(
		This 				: access Message_Agent_Type;
		Destination_Address : in String;
		Object 				: in String;
		Service 			: in String;
		Params 				: in YAMI.Parameters.Parameters_Collection;
		Callback			: access procedure(Content : in out YAMI.Parameters.Parameters_Collection));

	procedure Process_Reply(Content : in out YAMI.Parameters.Parameters_Collection);

	procedure Listen_To(Server_Address : String);

	procedure Close(This: access Message_Agent_Type);

	procedure Init;--return Message_Agent_Ref;

	Instance 				: Message_Agent_Ref := null;

private

	type Message_Agent_Type(Options : Parameters_Collection_Access) is tagged limited record
		Client_Agent : YAMI.Agents.Agent_Access := YAMI.Agents.New_Agent(Options.all);
	end record;

end Message_Agent;
