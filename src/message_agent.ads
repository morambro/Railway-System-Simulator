--==============================================================================
-- Created by:
--		Moreno Ambrosin
--  	Mat.  : 1035635
-- Date:
-- 		08/03/2013
--==============================================================================

with YAMI.Outgoing_Messages; use YAMI.Outgoing_Messages;
with YAMI.Agents;
with YAMI.Parameters;
with Unchecked_Deallocation;

package Message_Agent is

	type Message_Agent_Type is tagged limited private;

	type Message_Agent_Ref is access all Message_Agent_Type;

	procedure Send(
		This 				: access Message_Agent_Type;
		Destination_Address : in String;
		Object 				: in String;
		Service 			: in String;
		Params 				: in YAMI.Parameters.Parameters_Collection);

	procedure Process_Reply(Content : in out YAMI.Parameters.Parameters_Collection);

	procedure Close(This: access Message_Agent_Type);


private

	type Message_Agent_Type is tagged limited record
		Client_Agent : YAMI.Agents.Agent_Access := YAMI.Agents.New_Agent;
	end record;

end Message_Agent;