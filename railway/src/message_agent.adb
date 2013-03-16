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
	 	State : constant String := Content.Get_String("address");
	begin
		Put_Line("Result : " & State);
	end Process_Reply;

	procedure Send(
		This 				: access Message_Agent_Type;
		Destination_Address : in String;
		Object 				: in String;
		Service 			: in String;
		Params 				: in YAMI.Parameters.Parameters_Collection)
	is
		Msg : aliased YAMI.Outgoing_Messages.Outgoing_Message;
      	State : YAMI.Outgoing_Messages.Message_State;
	begin
		--  the "content" field name is arbitrary,
	    --  but needs to be recognized at the server side
        This.Client_Agent.Send(
        Destination_Address, Object,Service, Params,Msg'Unchecked_Access);

        Msg.Wait_For_Completion;

      	State := Msg.State;

		if State = YAMI.Outgoing_Messages.Replied then

        	Msg.Process_Reply_Content(Process_Reply'Access);

      	elsif State = YAMI.Outgoing_Messages.Rejected then
         	Put_Line("The message has been rejected: " & Msg.Exception_Message);
      	else
         	Put_Line ("The message has been abandoned.");
      	end if;

    end Send;


    procedure Close(This: access Message_Agent_Type) is
    begin
    	Logger.Log(
    		Sender => NAME & ".Message_Agent",
    		Message => "Closing the agent",
    		L => Logger.DEBUG);
		YAMI.Agents.Free(This.Client_Agent);
    end Close;


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

