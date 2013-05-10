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
--  Railway_Simulation is distributed in the hope that it will be useful,		--
--  but WITHOUT ANY WARRANTY; without even the implied warranty of				--
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the				--
--  GNU General Public License for more details.								--
--																				--
--  You should have received a copy of the GNU General Public License			--
--  along with Railway_Simulation.  If not, see <http://www.gnu.org/licenses/>. --
----------------------------------------------------------------------------------
with Logger;
with Environment;
with Trains;
with YAMI.Parameters;
with Message_Agent;
with Ada.Exceptions;
with Ticket;
with Train;
with Traveler;
with Name_Server_Interface;

package body Remote_Station_Interface is

	procedure Send_Train(
		Train_Descriptor_Index 	: in	 Positive;
		Station	 				: in	 Positive;
		Platform				: in 	 Positive;
		Next_Node_Name			: in 	 String) is
	begin
		-- # First, Resolve destination address
		declare

			-- #
			-- # Callback function, used to handle the response from the Name Server Interface
			-- #
			procedure Get_Results(
				Address : in	 String) is
			begin
				if Address /= "_" then
					-- # If the Address is a valid address, send a message to transfer the Train
					declare
						Parameters : YAMI.Parameters.Parameters_Collection := YAMI.Parameters.Make_Parameters;
					begin
						Logger.Log(
							Sender 	=> "Remote_Station_Interface.Send_Train",
							Message => "The destination is located at address " & Address,
							L 		=> Logger.DEBUG);

						-- # Once the address is resolved, send the Train to the destination node, and interrupt
						-- # the current task Train execution.

						Parameters.Set_String("station",Integer'Image(Station));
						Parameters.Set_String("platform",Integer'Image(Platform));
						Parameters.Set_String("train_index",Integer'Image(Train_Descriptor_Index));
						-- # Pass also current time table index and position
						Parameters.Set_String("current_run",
							Integer'Image(Environment.Route_Time_Table(Trains.Trains(Train_Descriptor_Index).Route_Index).Current_Run));
						Parameters.Set_String("current_run_position",
							Integer'Image(Environment.Route_Time_Table(Trains.Trains(Train_Descriptor_Index).Route_Index).Current_Run_Cursor));
						Parameters.Set_String("train",Train.Get_Json(Trains.Trains(Train_Descriptor_Index)));

						Message_Agent.Instance.Send(
							Destination_Address => Address,
							Object 				=> "message_handler",
							Service 			=> "train_transfer",
							Params 				=> Parameters,
							Callback			=> null
						);
				    end;
				end if;
		    end Get_Results;

		begin
			-- # Ask the Name Server to Resolve the given destination.
			Name_Server_Interface.Resolve(
				Name_Server		=> Environment.Get_Name_Server,
				Node_Name		=> Next_Node_Name,
				Callback 		=> Get_Results'Access);
		exception
			when E : others =>
				Logger.Log(
	   				Sender => "Remote_Station_Interface.Send_Train",
	   				Message => "ERROR : Exception: " & Ada.Exceptions.Exception_Name(E) & "  " & Ada.Exceptions.Exception_Message(E),
	   				L => Logger.ERROR);
		end;

		-- # Stop current execution raising an exception.
		raise Stop_Train_Execution;

    end Send_Train;


	procedure Train_Left_Message(
		Train_Descriptor_Index 	: in	 Positive;
		Station	 				: in	 Positive;
		Platform				: in 	 Positive;
		Node_Name				: in	 String)
	is
		-- #
		-- # Callback function, used to handle the response from the Name Server Interface
		-- #
		procedure Get_Results(
			Address : in	 String) is
		begin
			if Address /= "_" then
				declare
					Parameters : YAMI.Parameters.Parameters_Collection := YAMI.Parameters.Make_Parameters;

				begin
					Logger.Log(
						Sender 	=> "Remote_Station_Interface.Send_Ack",
						Message => "The destination is located at address " & Address,
						L 		=> Logger.DEBUG
					);


					-- # Once the address is resolved, send the Train to the destination node, and interrupt
					-- # the current task Train execution.

					Parameters.Set_String("station",Integer'Image(Station));
					Parameters.Set_String("platform",Integer'Image(Platform));
					Parameters.Set_String("train_index",Integer'Image(Train_Descriptor_Index));

					Message_Agent.Instance.Send(
						Destination_Address => Address,
						Object 				=> "message_handler",
						Service 			=> "train_left_platfrom",
						Params 				=> Parameters,
						Callback			=> null
					);
				end;
			end if;
		end Get_Results;

	begin
		-- # Ask the Name Server to Resolve the given destination.
		Name_Server_Interface.Resolve(
			Name_Server		=> Environment.Get_Name_Server,
			Node_Name		=> Node_Name,
			Callback 		=> Get_Results'Access);
	exception
		when E : others =>
			Logger.Log(
   				Sender => "Remote_Station_Interface.Send_Train",
   				Message => "ERROR : Exception: " & Ada.Exceptions.Exception_Name(E) & "  " & Ada.Exceptions.Exception_Message(E),
   				L => Logger.ERROR);
    end Train_Left_Message;



    procedure Send_Traveler_To_Leave(
		Traveler_Index	: in	 Positive;
		Train_ID		: in 	 Positive;
		Station	 		: in	 Positive;
		Platform		: in 	 Positive;
		Node_Name		: in	 String )
	is

		-- #
		-- # Callback function, used to handle the response from the Name Server Interface
		-- #
		procedure Get_Results(
			Address : in	 String) is
		begin
			if Address /= "_" then
				declare
					Parameters : YAMI.Parameters.Parameters_Collection := YAMI.Parameters.Make_Parameters;
				begin
					Logger.Log(
						Sender 	=> "Remote_Station_Interface.Send_Traveler_To_Leave",
						Message => "The destination is located at address " & Address,
						L 		=> Logger.DEBUG
					);

					Parameters.Set_String("station",Integer'Image(Station));
					Parameters.Set_String("platform",Integer'Image(Platform));
					Parameters.Set_String("traveler_index",Integer'Image(Traveler_Index));
					Parameters.Set_String("train_id",Integer'Image(Train_ID));
					Parameters.Set_String("traveler",Traveler.Get_Json(Environment.Travelers(Traveler_Index)));
					Parameters.Set_String("ticket",Ticket.To_Json(Environment.Travelers(Traveler_Index).The_Ticket));

					Message_Agent.Instance.Send(
						Destination_Address => Address,
						Object 				=> "message_handler",
						Service 			=> "traveler_leave_transfer",
						Params 				=> Parameters,
						Callback			=> null
					);
				end;
			end if;
	    end Get_Results;

	begin
		-- # Ask the Name Server to Resolve the given destination.
		Name_Server_Interface.Resolve(
			Name_Server		=> Environment.Get_Name_Server,
			Node_Name		=> Node_Name,
			Callback 		=> Get_Results'Access);
	exception
		when E : others =>
			Logger.Log(
   				Sender => "Remote_Station_Interface.Send_Traveler_To_Leave",
   				Message => "ERROR : Exception: " & Ada.Exceptions.Exception_Name(E) & "  " & Ada.Exceptions.Exception_Message(E),
   				L => Logger.ERROR);


    end Send_Traveler_To_Leave;


	procedure Wait_For_Train_To_Arrive(
		Next_Station 				: in 	 Positive;
		Traveler_Manager_Index		: in 	 Positive;
		Train_ID					: in 	 Positive;
		Destination_Platform_Index	: in 	 Positive;
		Next_Region					: in 	 String)
	is

		-- #
		-- # Callback function, used to handle the response from the Name Server Interface
		-- #
		procedure Get_Results(
			Address : in	 String) is
		begin

			if Address /= "_" then
				declare
					Parameters : YAMI.Parameters.Parameters_Collection := YAMI.Parameters.Make_Parameters;
				begin
					Logger.Log(
						Sender 	=> "Remote_Station_Interface.Wait_For_Train_To_Arrive",
						Message => "The destination is located at address " & Address,
						L 		=> Logger.DEBUG
					);

					Parameters.Set_String("station",Integer'Image(Next_Station));
					Parameters.Set_String("traveler_index",Integer'Image(Traveler_Manager_Index));
					Parameters.Set_String("train_id",Integer'Image(Train_ID));
					Parameters.Set_String("platform",Integer'Image(Destination_Platform_Index));
					Parameters.Set_String("traveler",Traveler.Get_Json(Environment.Travelers(Traveler_Manager_Index)));
					Parameters.Set_String("ticket",Ticket.To_Json(Environment.Travelers(Traveler_Manager_Index).The_Ticket));

					Message_Agent.Instance.Send(
						Destination_Address => Address,
						Object 				=> "message_handler",
						Service 			=> "traveler_enter_transfer",
						Params 				=> Parameters,
						Callback			=> null
					);
				end;

			end if;

	    end Get_Results;


		Parameters	: YAMI.Parameters.Parameters_Collection := YAMI.Parameters.Make_Parameters;
	begin
		-- # Ask the Name Server to Resolve the given destination.
		Name_Server_Interface.Resolve(
			Name_Server		=> Environment.Get_Name_Server,
			Node_Name		=> Next_Region,
			Callback 		=> Get_Results'Access);
	exception
		when E : others =>
			Logger.Log(
   				Sender => "Remote_Station_Interface.Wait_For_Train_To_Arrive",
   				Message => "ERROR : Exception " & Ada.Exceptions.Exception_Name(E) & "  " & Ada.Exceptions.Exception_Message(E),
   				L => Logger.ERROR);
    end Wait_For_Train_To_Arrive;


    -- ##############################################################################################
end Remote_Station_Interface;