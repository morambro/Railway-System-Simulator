--==============================================================================
-- Created by:
--		Moreno Ambrosin
--  	Mat.  : 1035635
-- Date:
-- 		08/03/2013
--==============================================================================
with Ada.Text_IO;

package body Message_Agent is

	 procedure Process_Reply(Content : in out YAMI.Parameters.Parameters_Collection) is
	 	State : constant String := Content.Get_String("address");
	 begin
		Ada.Text_IO.Put_Line("Result : " & State);
	 end Process_Reply;

	procedure Send(This : access Message_Agent_Type;Dest : String; Message : String) is
		Params : YAMI.Parameters.Parameters_Collection := YAMI.Parameters.Make_Parameters;
		Msg : aliased YAMI.Outgoing_Messages.Outgoing_Message;
      	State : YAMI.Outgoing_Messages.Message_State;
	begin
		--  the "content" field name is arbitrary,
	    --  but needs to be recognized at the server side
	    Params.Set_String ("name", Message);
        Params.Set_String ("address", "tcp://address");
        This.Client_Agent.Send (Dest, "name_server", "add", Params,Msg'Unchecked_Access);

        Msg.Wait_For_Completion;

      	State := Msg.State;

		if State = YAMI.Outgoing_Messages.Replied then

        	Msg.Process_Reply_Content(Process_Reply'Access);

      	elsif State = YAMI.Outgoing_Messages.Rejected then
         	Ada.Text_IO.Put_Line("The message has been rejected: " & Msg.Exception_Message);
      	else
         	Ada.Text_IO.Put_Line ("The message has been abandoned.");
      	end if;



		Params.Set_String ("name", "yo");
        Params.Set_String ("address", "tcp://address");
        This.Client_Agent.Send (Dest, "name_server", "get", Params,Msg'Unchecked_Access);

        Msg.Wait_For_Completion;

      	State := Msg.State;

		if State = YAMI.Outgoing_Messages.Replied then
        	Msg.Process_Reply_Content(Process_Reply'Access);

      	elsif State = YAMI.Outgoing_Messages.Rejected then
         	Ada.Text_IO.Put_Line("The message has been rejected: " & Msg.Exception_Message);
      	else
         	Ada.Text_IO.Put_Line ("The message has been abandoned.");
		end if;

    end Send;


end Message_Agent;

--	declare
--	  	Server_Address : constant String := Ada.Command_Line.Argument (1);
--	  	Client_Agent : YAMI.Agents.Agent := YAMI.Agents.Make_Agent;
--	begin
--		--  read lines of text from standard input
--		--  and post each one for transmission
--    	while not Ada.Text_IO.End_Of_File loop
--       		declare
--				Input_Line : constant String := Ada.Text_IO.Get_Line;
--				Params : YAMI.Parameters.Parameters_Collection := YAMI.Parameters.Make_Parameters;
--			begin
--		        --  the "content" field name is arbitrary,
--		        --  but needs to be recognized at the server side
--		        Params.Set_String ("content", Input_Line);
--		        Params.Set_String ("prova", "Sciao belo");
--		        Client_Agent.Send_One_Way (Server_Address, "printer", "print", Params);
--		    end;
--      	end loop;
--	end;
