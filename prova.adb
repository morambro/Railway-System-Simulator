with YAMI.Outgoing_Messages; use YAMI.Outgoing_Messages;

procedure Prova is 

	Message : aliased Outgoing_Message;
   State : Message_State;

   procedure Process_Reply
     (Reply_Content : in out Parameters_Collection) is
   begin
      Put_Line("Replied");
   end Process_Reply;

begin

   Client_Agent.Send
     ("tcp://somewhere:12345", "my_object", "do_something",
      Content, Message'Unchecked_Access);

   --   do something else while the message makes progress
   --   or synchronize with its completion:
   Message.Wait_For_Completion;

   State := Message.State;
   if State = Replied then

      Message.Process_Reply_Content (Process_Reply'Access);

   elsif State = Rejected then
      --  Message.Exception_Message is an error's description
		Put_Line("Rejected");
   else
      --  the message has been abandoned,
      --  because the given channel has been closed
  		Put_Line("Abandoned");
   end if;

exception
   when Runtime_Error =>
      --  message could not be sent because the connection
      --  was not established with remote agent
      Put_Line("Runtime error");
end Prova;
