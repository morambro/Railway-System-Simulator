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
with Ada.Unchecked_Deallocation;

package Message_Agent is

	procedure Send(
		Destination_Address : String;
		Object : String;
		Service : String;
		Params : YAMI.Parameters.Parameters_Collection);

	procedure Process_Reply
        (Content : in out YAMI.Parameters.Parameters_Collection);

end Message_Agent;