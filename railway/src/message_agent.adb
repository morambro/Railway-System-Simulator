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

with Ada.Text_IO;use Ada.Text_IO;
with Ada.Exceptions;  use Ada.Exceptions;
with Logger;

package body Message_Agent is

	NAME : constant String := "Message_Agent";

	procedure Process_Reply(Content : in out YAMI.Parameters.Parameters_Collection) is
	 	State : constant String := Content.Get_String("response");
	begin
		Logger.Log(
			Sender	=> "Message_Agent.Default_Handler",
			Message	=> "Result : " & State,
			L		=> Logger.DEBUG);
	end Process_Reply;

	procedure Send(
		This 				: access Message_Agent_Type;
		Destination_Address : in String;
		Object 				: in String;
		Service 			: in String;
		Params 				: in YAMI.Parameters.Parameters_Collection;
		Callback			: access procedure(Content : in out YAMI.Parameters.Parameters_Collection))
	is
		Msg : aliased YAMI.Outgoing_Messages.Outgoing_Message;
      	State : YAMI.Outgoing_Messages.Message_State;
	begin
		--  the "content" field name is arbitrary,
	    --  but needs to be recognized at the server side
        This.Client_Agent.Send(
        	Destination_Address,
        	Object,
        	Service,
        	Params,
        	Msg'Unchecked_Access);

        Msg.Wait_For_Completion;

      	State := Msg.State;

		if State = YAMI.Outgoing_Messages.Replied then

        if Callback /= null then
        	Msg.Process_Reply_Content(Callback);
		else
			Msg.Process_Reply_Content(Process_Reply'Access);
		end if;

      	elsif State = YAMI.Outgoing_Messages.Rejected then
         	Put_Line("The message has been rejected: " & Msg.Exception_Message);
      	else
         	Put_Line ("The message has been abandoned.");
      	end if;

    end Send;


	procedure Send_One_Way(
		This 				: access Message_Agent_Type;
		Destination_Address : in String;
		Object 				: in String;
		Service 			: in String;
		Params 				: in YAMI.Parameters.Parameters_Collection)
	is
	begin
		--  the "content" field name is arbitrary,
	    --  but needs to be recognized at the server side
        This.Client_Agent.Send_One_Way(
        	Destination_Address,
        	Object,
        	Service,
        	Params);

    end Send_One_Way;



    procedure Close(This: access Message_Agent_Type) is
    begin
		YAMI.Agents.Free(This.Client_Agent);
		Logger.Log(
    		Sender => NAME & ".Message_Agent",
    		Message => "Closing the agent",
    		L => Logger.DEBUG);
    exception
			-- # If an error occurs, send an ERROR message
			when E : others =>
				declare
				begin
					Logger.Log(
						Sender => "Termination_Handler",
						Message => "ERROR : Exception: " & Ada.Exceptions.Exception_Name(E) & "  " & Ada.Exceptions.Exception_Message(E),
						L => Logger.ERROR);
				end;
    end Close;



	procedure Listen_To(
		This 				: access Message_Agent_Type;
		Server_Address 		: in	 String)
	is

  		Resolved_Server_Address 		: String (1 .. YAMI.Agents.Max_Target_Length);
  		Resolved_Server_Address_Last 	: Natural;
   	begin
    	This.Client_Agent.Add_Listener
       		(Server_Address,
         	Resolved_Server_Address,
         	Resolved_Server_Address_Last);

         Put_Line("Server listening on " & Resolved_Server_Address(1..Resolved_Server_Address_Last));

         This.Client_Agent.Register_Object("message_handler", Main_Handler'Unchecked_Access);

    end Listen_To;




	procedure Add_Handler(
			This				: access Message_Agent_Type;
			Service				: in String;
			The_Handler			: Handler) is
	begin
		This.Handlers_Map.Insert(Service,The_Handler);
    end Add_Handler;




	procedure Init is
		Options : Parameters_Collection_Access := New_Parameters;
	begin
	   	Options.Set_Integer (Dispatcher_Threads, 5);
		Instance := new Message_Agent_Type(Options);
		Main_Handler.Agent := Instance;
    end Init;




    overriding procedure Call(
    	This	: in	out Main_Message_Handler;
  		Message : in	out YAMI.Incoming_Messages.Incoming_Message'Class)
    is

		Message_Type : String :=  Message.Message_Name;

    begin

		if This.Agent.Handlers_Map.Contains(Message_Type) then
			Ada.Text_IO.Put_Line( "Contains handler for type " & Message_Type);
			-- # Invoke the correct handler for the given message type
			declare
				Proced : Handler := This.Agent.Handlers_Map(Message_Type);
			begin
				Proced(Message);
			end;

		else
			Logger.Log(
   				Sender 	=> "Main_Handler",
   				Message => "No Handler for the given message type : " & Message_Type & "!",
   				L 		=> Logger.ERROR);
		end if;

   	exception
			when E : others =>
			Logger.Log(
   				Sender => "",
   				Message => "ERROR : Exception: " & Ada.Exceptions.Exception_Name(E) & "  " & Ada.Exceptions.Exception_Message(E),
   				L => Logger.ERROR);
    end Call;


end Message_Agent;

