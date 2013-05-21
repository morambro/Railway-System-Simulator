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
with Ada.Text_IO;use Ada.Text_IO;
with Ada.Strings.Unbounded;

with Environment;
with Trains;
with Segments;
with Routes;
with Train_Pool;
with Traveler_Pool;

with Logger;
with Message_Agent;

with Traveler;

with Regional_Ticket_Office;

with Ada.Exceptions;  use Ada.Exceptions;

with Handlers;
with Name_Server_Interface;

with Central_Controller_Interface;

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
  		"  6) Central controller address [ -cc ] (e.g. -ct tcp://...)" & ASCII.LF &
  		"  7) Low Train Task Pool dimention [ -lpd ] (e.g. -lpd 5)" & ASCII.LF &
  		"  8) Central controller address [ -hpd ] (e.g. -hpd 5)" & ASCII.LF &
  	"------------------------------------------------------------------------";

begin

	if Ada.Command_Line.Argument_Count < 15 then
		Ada.Text_IO.Put_Line("ERROR : Not enougth arguments!");
		Ada.Text_IO.Put_Line(INSTRUCTIONS);
		Ada.Command_Line.Set_Exit_Status(Ada.Command_Line.Failure);
		return;
	end if;

	declare

		Log_Level 	: constant String 	:= Ada.Command_Line.Argument (1);
		Par_1 	  	: constant String 	:= Ada.Command_Line.Argument (2);
		Name_Server : aliased String 	:= Ada.Command_Line.Argument (3);
		Par_2 	  	: constant String 	:= Ada.Command_Line.Argument (4);
		Node_Name  	: aliased String 	:= Ada.Command_Line.Argument (5);
		Par_3 	  	: constant String 	:= Ada.Command_Line.Argument (6);
		Node_Addr  	: aliased String 	:= Ada.Command_Line.Argument (7);
		Par_4 	  	: constant String 	:= Ada.Command_Line.Argument (8);
		Central_T  	: constant String 	:= Ada.Command_Line.Argument (9);
		Par_5	  	: constant String 	:= Ada.Command_Line.Argument (10);
		Central_C  	: constant String 	:= Ada.Command_Line.Argument (11);
		Par_6	  	: constant String 	:= Ada.Command_Line.Argument (12);
		L_Task_Num	: constant Integer	:= Integer'Value(Ada.Command_Line.Argument(13));
		Par_7	  	: constant String 	:= Ada.Command_Line.Argument (14);
		H_Task_Num 	: constant Integer 	:= Integer'Value(Ada.Command_Line.Argument(15));

	begin
		-- # Parameters correctness check.

		if(not Logger.Init(Log_Level)) then
			Logger.Log(
				Sender 	=> "Main",
				Message =>"ERROR : Unknown log level " & Log_Level & "! Use a valid log level [ -i | -n | -d ].",
				L 		=> Logger.ERROR);

			Ada.Command_Line.Set_Exit_Status(Ada.Command_Line.Failure);
			return;
		end if;

		if Par_1 /= "-ns" then
			Logger.Log(
				Sender 	=> "Main",
				Message => "ERROR : name server address MUST be specified, invalid option " & Par_1,
				L 		=> Logger.ERROR);
			Ada.Command_Line.Set_Exit_Status(Ada.Command_Line.Failure);
			return;
		end if;

		if Par_2 /= "-nn" then
			Logger.Log(
				Sender 	=> "Main",
				Message => "ERROR : node name MUST be specified, invalid option " & Par_2,
				L 		=> Logger.ERROR);
			Ada.Command_Line.Set_Exit_Status(Ada.Command_Line.Failure);
			return;
		end if;

		if Par_3 /= "-na" then
			Logger.Log(
				Sender 	=> "Main",
				Message => "ERROR : node address MUST be specified, invalid option " & Par_3,
				L 		=> Logger.ERROR);
			Ada.Command_Line.Set_Exit_Status(Ada.Command_Line.Failure);
			return;
		end if;

		if Par_4 /= "-ct" then
			Logger.Log(
				Sender 	=> "Main",
				Message => "ERROR : central ticket office address MUST be specified, invalid option " & Par_4,
				L 		=> Logger.ERROR);
			Ada.Command_Line.Set_Exit_Status(Ada.Command_Line.Failure);
			return;
		end if;

		if Par_5 /= "-cc" then
			Logger.Log(
				Sender 	=> "Main",
				Message => "ERROR : central controller address MUST be specified, invalid option " & Par_5,
				L 		=> Logger.ERROR);
			Ada.Command_Line.Set_Exit_Status(Ada.Command_Line.Failure);
			return;
		end if;

		if Par_6 /= "-lpd" then
			Logger.Log(
				Sender 	=> "Main",
				Message => "ERROR : Low Priority Task Pool Dimension MUST be specified, invalid option " & Par_6,
				L 		=> Logger.ERROR);
			Ada.Command_Line.Set_Exit_Status(Ada.Command_Line.Failure);
			return;
		end if;

		if Par_7 /= "-hpd" then
			Logger.Log(
				Sender 	=> "Main",
				Message => "ERROR : High Priority Task Pool Dimension MUST be specified, invalid option " & Par_7,
				L 		=> Logger.ERROR);
			Ada.Command_Line.Set_Exit_Status(Ada.Command_Line.Failure);
			return;
		end if;

		-- # Now Initializations can be made.
		begin
			Message_Agent.Init;
			Message_Agent.Instance.Listen_To(Node_Addr);
			Message_Agent.Instance.Add_Handler("start_simulation",Handlers.Perform_Initializations_Handler'Access);
			Message_Agent.Instance.Add_Handler("train_transfer",Handlers.Station_Train_Transfer_Handler'Access);
			Message_Agent.Instance.Add_Handler("traveler_leave_transfer",Handlers.Station_Traveler_Leave_Transfer_Handler'Access);
			Message_Agent.Instance.Add_Handler("traveler_enter_transfer",Handlers.Station_Traveler_Enter_Transfer_Handler'Access);
			Message_Agent.Instance.Add_Handler("train_left_platfrom",Handlers.Station_Train_Transfer_Left_Handler'Access);
			Message_Agent.Instance.Add_Handler("ticket_creation",Handlers.Get_Ticket_Handler'Access);
			Message_Agent.Instance.Add_Handler("is_present",Handlers.Is_Station_Present_Handler'Access);
			Message_Agent.Instance.Add_Handler("ticket_ready",Handlers.Ticket_Ready_Handler'Access);
			Message_Agent.Instance.Add_Handler("terminate",Handlers.Termination_Handler'Access);

			-- # Register to the Name Server
			Name_Server_Interface.Bind(
				Name_Server  => Name_Server'Access,
				Node_Name 	 => Node_Name'Access,
				Address 	 => Node_Addr'Access);

			declare
				-- # Create the two Task Pool, to execute Trains and Travelers operations.
				Traveler_Tasks 	: Traveler_Pool.Traveler_Pool_Type(5);
				Pool			: Train_Pool.Train_Task_Pool(L_Task_Num,H_Task_Num);

			-- # This block will be in co-begin with the Tasks in Traveler_Pool and Train_Pool
			begin

				-- # Perform initialization of the Environment
				Environment.Init(Node_Name,Name_Server,Central_T,Central_C);

				-- # Perform initialization of all the Segments
				Segments.Init;

				-- # Perform initialization of all the Routes
				Routes.Init;

				-- # Perform initialization of the Regional Ticket Office
				Regional_Ticket_Office.Init("../configuration/" & Node_Name & "-paths.json");

			exception
				when E : others =>
					-- # In case of an Error, terminate all and close
					Logger.Log(
		   				Sender => "Main",
		   				Message => "ERROR : Exception: " & Ada.Exceptions.Exception_Name(E) & "  " & Ada.Exceptions.Exception_Message(E),
		   				L => Logger.ERROR);
					Traveler_Pool.Stop;
					Train_Pool.Stop;
			end;

			-- # At this point, all the Tasks will be out of scope, so we can Terminate in peace.
			-- # Notify termination to the Central Controller.
			Central_Controller_Interface.Notify_Termination;

			-- # Remove to Name Server.
			Name_Server_Interface.Remove(
				Name_Server  => Name_Server'Access,
				Node_Name 	 => Node_Name'Access);

			-- # Finally close Message Agent
			Message_Agent.Instance.Close;

		exception
   			when E : Name_Server_Interface.Name_Server_Exception =>
   				Logger.Log(
	   				Sender => "Main",
	   				Message => "ERROR : " & Exception_Message(E) & " Impossibile connettersi al Name Server",
	   				L => Logger.ERROR);
	   			-- # Close the Message Agent before exit.
	   			Message_Agent.Instance.Close;

	   		when Error : others =>
		   		Logger.Log(
	   				Sender => "Main",
	   				Message => "ERROR : " & Ada.Exceptions.Exception_Name(Error) & " " & Ada.Exceptions.Exception_Message(Error),
	   				L => Logger.ERROR);
	   			-- # Close the Message Agent before exit.
	   			Message_Agent.Instance.Close;
		end;
	end;
exception
	when E : others => Ada.Text_IO.Put_Line(Ada.Exceptions.Exception_Message (E));
end Main;
