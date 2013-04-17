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

with Ada.Command_Line;
with Ada.Exceptions;
with Ada.Text_IO;
with Ada.Strings.Unbounded;

with Environment;
with Trains;
with Segments;
with Train_Pool;
With Task_Pool;

with Logger;
with Message_Agent;
with YAMI.Parameters;

with Traveler;

with Ticket_Office;

with Ada.Exceptions;  use Ada.Exceptions;

with Handlers;

procedure Main is

	use Message_Agent;

	INSTRUCTIONS : constant String :=
	"------------------------------------------------------------------------" & ASCII.LF &
	"The following parameters must be specified:" & ASCII.LF &
  		"  1) Log level, chosen from [ -i | -n | -d ] \n" & ASCII.LF &
  		"  2) Name Server tcp Address [ -ns ] (e.g. -ns tcp://localhost:9000)" & ASCII.LF &
  		"  3) Node name identifier [ -nn ] (e.g. -nn Node1) " & ASCII.LF &
  		"  4) Node address [ -na ] (e.g. -na tcp://...)" & ASCII.LF &
  		"  5) Central ticket office address [ -ct ] (e.g. -ct tcp://...)" & ASCII.LF &
  	"------------------------------------------------------------------------";

begin

	if Ada.Command_Line.Argument_Count < 7 then
		-- ./run -name Node_Name -address tcp_//... -ns_address tcp://name_server_address [ -i | -n | -d ]
		Ada.Text_IO.Put_Line("ERROR : Not enougth arguments!");
		Ada.Text_IO.Put_Line(INSTRUCTIONS);
		Ada.Command_Line.Set_Exit_Status(Ada.Command_Line.Failure);
		return;
	end if;

	declare

		Log_Level 	: constant String := Ada.Command_Line.Argument (1);
		Par_1 	  	: constant String := Ada.Command_Line.Argument (2);
		Name_Server : constant String := Ada.Command_Line.Argument (3);
		Par_2 	  	: constant String := Ada.Command_Line.Argument (4);
		Node_Name  	: constant String := Ada.Command_Line.Argument (5);
		Par_3 	  	: constant String := Ada.Command_Line.Argument (6);
		Node_Addr  	: constant String := Ada.Command_Line.Argument (7);
		Par_4 	  	: constant String := Ada.Command_Line.Argument (8);
		Central_T  	: constant String := Ada.Command_Line.Argument (9);

	begin

		if(not Logger.Init(Log_Level)) then
			Ada.Text_IO.Put_Line("ERROR : Unknown log level " & Log_Level & "! Use a valid log level [ -i | -n | -d ].");
			Ada.Command_Line.Set_Exit_Status(Ada.Command_Line.Failure);
			return;
		end if;

		if Par_1 /= "-ns" then
			Ada.Text_IO.Put_Line("ERROR : You must specify name server address, invalid option " & Par_1);
			Ada.Command_Line.Set_Exit_Status(Ada.Command_Line.Failure);
			return;
		end if;

		if Par_2 /= "-nn" then
			Ada.Text_IO.Put_Line("ERROR : You must specify node name, invalid option " & Par_2);
			Ada.Command_Line.Set_Exit_Status(Ada.Command_Line.Failure);
			return;
		end if;

		if Par_3 /= "-na" then
			Ada.Text_IO.Put_Line("ERROR : You must specify node address, invalid option " & Par_3);
			Ada.Command_Line.Set_Exit_Status(Ada.Command_Line.Failure);
			return;
		end if;

		if Par_4 /= "-ct" then
			Ada.Text_IO.Put_Line("ERROR : You must specify central ticket office address, invalid option " & Par_4);
			Ada.Command_Line.Set_Exit_Status(Ada.Command_Line.Failure);
			return;
		end if;

		declare
			-- Creation of Actors for Travelers
			Params : YAMI.Parameters.Parameters_Collection := YAMI.Parameters.Make_Parameters;

		begin
			Message_Agent.Init;
			Message_Agent.Instance.Listen_To(Node_Addr);
			Message_Agent.Instance.Add_Handler("train_transfer",Handlers.Station_Train_Transfer_Handler'Access);
			Message_Agent.Instance.Add_Handler("traveler_leave_transfer",Handlers.Station_Traveler_Leave_Transfer_Handler'Access);
			Message_Agent.Instance.Add_Handler("traveler_enter_transfer",Handlers.Station_Traveler_Enter_Transfer_Handler'Access);
			Message_Agent.Instance.Add_Handler("train_transfer_ack",Handlers.Station_Train_Transfer_Ack_Handler'Access);
			Message_Agent.Instance.Add_Handler("ticket_creation",Handlers.Get_Ticket_Handler'Access);
			Message_Agent.Instance.Add_Handler("is_present",Handlers.Is_Station_Present_Handler'Access);
			Message_Agent.Instance.Add_Handler("ticket_ready",Handlers.Ticket_Ready_Handler'Access);

			Params.Set_String("node_name",Node_Name);
			Params.Set_String("address",Node_Addr);

			Message_Agent.Instance.Send(
				Destination_Address => Name_Server,
				Object 				=> "name_server",
				Service 			=> "add",
				Params 				=> Params,
				Callback			=> null
			);

			declare
				-- Start the real simulation
				Traveler_Tasks 	: Task_Pool.Task_Pool_Type(5);
				Pool			: Train_Pool.Train_Task_Pool(3,5);


				procedure Start is
				begin

					for I in 1 ..  Trains.Trains'Length loop
						if Ada.Strings.Unbounded.To_String(Trains.Trains(I).Start_Node) = Node_Name then
							Train_Pool.Associate(I);
						end if;
					end loop;

					for I in 1 ..  Environment.Travelers'Length loop
						if Ada.Strings.Unbounded.To_String(Environment.Travelers(I).Start_Node) = Node_Name then
							Task_Pool.Execute(Environment.Operations(I)(Traveler.BUY_TICKET));
						end if;
					end loop;

				end Start;

			begin

				Environment.Init(Node_Name,Name_Server,Central_T);
				Segments.Init;
				Ticket_Office.Init_Path_Map("res/" & Node_Name & "-paths.json");


				Start;

				--Ticket_Office.Init;

--  				if Node_Name = "Node_1" then
--  					Train_Pool.Associate(2);
--  					Train_Pool.Associate(1);
--  				end if;

--  				delay 2.0;
--  				Train_Pool.Associate(3);
--  				Train_Pool.Associate(4);

--  				for I in 1 .. Routes.Get_Routes_Containing(1,2)'Length loop
--  					Ada.Text_IO.Put_Line(" *** Found : ");
--  					Route.Print(Routes.All_Routes(Routes.Get_Routes_Containing(1,2)(I)).all);
--  				end loop;


				--Ticket_Office.Get_Ticket(1,"G1","4");

--  				if Node_Name = "Node_1" then
--  					Task_Pool.Execute(Environment.Operations(1)(Traveler.LEAVE));
--  				end if;
--  				delay 4.0;
--
--  				Train_Pool.Stop;
--  				Task_Pool.Stop;
			exception
				when E : others =>
				Logger.Log(
	   				Sender => "",
	   				Message => "ERROR : Exception: " & Ada.Exceptions.Exception_Name(E) & "  " & Ada.Exceptions.Exception_Message(E),
	   				L => Logger.ERROR);
			end;
			Message_Agent.Instance.Close;

		exception
   			when E : YAMI.Runtime_Error =>
   				Logger.Log(
	   				Sender => "Message_Agent.Message_Agent_Type",
	   				Message => "ERROR : " & Exception_Message(E) & " Impossibile connettersi al Name Server",
	   				L => Logger.ERROR);
	   			Message_Agent.Instance.Close;
	   		when Error : others =>
		    -- Handle all others
		    	Ada.Text_IO.Put("Exception: ");
		    	Ada.Text_IO.Put_Line(Ada.Exceptions.Exception_Name(Error));
			    Ada.Text_IO.Put_Line(Ada.Exceptions.Exception_Message(Error));
		end;
	end;
exception
	when E : others => Ada.Text_IO.Put_Line(Ada.Exceptions.Exception_Message (E));
end Main;
