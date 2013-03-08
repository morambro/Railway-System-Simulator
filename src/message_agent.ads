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

package Message_Agent is

	type Message_Agent_Type is tagged private;

		procedure Send(
			This : access Message_Agent_Type;
			Dest : String;
			Message : String);

	procedure Process_Reply
        (Content : in out YAMI.Parameters.Parameters_Collection);

private

	type Message_Agent_Type is tagged record
		Client_Agent : YAMI.Agents.Agent_Access := YAMI.Agents.New_Agent;
	end record;

end Message_Agent;