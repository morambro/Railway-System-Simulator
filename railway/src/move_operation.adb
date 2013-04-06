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
with Ada.Strings.Unbounded;
with Environment;
with Logger;
with Ada.Exceptions;
with YAMI.Parameters;
with Ticket;
with Message_Agent;

package body Move_Operation is

	use Ada.Strings.Unbounded;

	NAME_LEAVE : constant String := "Move_Operation.Leave_Operation_Type";

	procedure Do_Operation(This : in Leave_Operation_Type) is
		Next_Stage 				: Positive 	:= Environment.Travelers(This.Traveler_Manager_Index).Ticket.Next_Stage;
		Start_Station 			: Natural 	:= Environment.Travelers(This.Traveler_Manager_Index).Ticket.Stages(Next_Stage).Start_Station;
		Train_ID	 			: Natural 	:= Environment.Travelers(This.Traveler_Manager_Index).Ticket.Stages(Next_Stage).Train_ID;
		Start_Platform_Index 	: Natural 	:= Environment.Travelers(This.Traveler_Manager_Index).Ticket.Stages(Next_Stage).Start_Platform_Index;
	begin
		Logger.Log(
			Sender 	=> NAME_LEAVE,
			Message => "Traveler" & Integer'Image(Environment.Travelers(This.Traveler_Manager_Index).Traveler.ID) &
					   " will wait to LEAVE at platform" & Integer'Image(Start_Platform_Index) &
					   ", station" & Integer'Image(Start_Station),
			L 		=> Logger.NOTICE);

		Environment.Stations(Start_Station).Wait_For_Train_To_Go(
			Outgoing_Traveler 	=> This.Traveler_Manager_Index,
			Train_ID 			=> Train_ID,
			Platform_Index		=> Start_Platform_Index);

	exception
		when Error : others =>
			Logger.Log(
				NAME_LEAVE,
				"Exception: " & Ada.Exceptions.Exception_Name(Error) & " , " & Ada.Exceptions.Exception_Message(Error),
				Logger.ERROR
			);
	end Do_Operation;


	-- #
	-- # Operation which lets the Traveler (Manager)
	-- #
	procedure Do_Operation(This : in Enter_Operation_Type) is
		Next_Stage 					: Positive 			:= Environment.Travelers(This.Traveler_Manager_Index).Ticket.Next_Stage;
		Next_Station 				: Natural 			:= Environment.Travelers(This.Traveler_Manager_Index).Ticket.Stages(Next_Stage).Next_Station;
		Next_Region 				: Unbounded_String 	:= Environment.Travelers(This.Traveler_Manager_Index).Ticket.Stages(Next_Stage).Region;
		Train_ID	 				: Natural 			:= Environment.Travelers(This.Traveler_Manager_Index).Ticket.Stages(Next_Stage).Train_ID;
		Destination_Platform_Index 	: Natural 			:= Environment.Travelers(This.Traveler_Manager_Index).Ticket.Stages(Next_Stage).Destination_Platform_Index;
	begin
		Logger.Log(
			Sender 	=> NAME_LEAVE,
			Message => "Traveler" & Integer'Image(Environment.Travelers(This.Traveler_Manager_Index).Traveler.ID) &
					   " will wait to ARRIVE at platform" & Integer'Image(Destination_Platform_Index) & ", station " & Integer'Image(Next_Station),
			L 		=> Logger.NOTICE);

		-- # Check if the next destination is in the current Region or not
		if Next_Region = Environment.Get_Node_Name then
			Environment.Stations(Next_Station).Wait_For_Train_To_Arrive(
				Incoming_Traveler 	=> This.Traveler_Manager_Index,
				Train_ID 			=> Train_ID,
				Platform_Index		=> Destination_Platform_Index);
		else
			declare

					-- # Callback function, used to handle the response
				procedure Get_Results(Content : in out YAMI.Parameters.Parameters_Collection) is

					Address : String := Content.Get_String("response");
					Parameters : YAMI.Parameters.Parameters_Collection := YAMI.Parameters.Make_Parameters;

				begin
					Logger.Log(
						Sender 	=> "Move_Operation",
						Message => "The destination is located at address " & Address,
						L 		=> Logger.DEBUG
					);

					if Address /= "_" then

						Parameters.Set_String("station",Integer'Image(Next_Station));
						Parameters.Set_String("traveler_index",Integer'Image(This.Traveler_Manager_Index));
						Parameters.Set_String("train_id",Integer'Image(Train_ID));
						Parameters.Set_String("platform",Integer'Image(Destination_Platform_Index));
						Parameters.Set_String("traveler",Traveler.Get_Json(Environment.Travelers(This.Traveler_Manager_Index)));
						Parameters.Set_String("ticket",Ticket.Get_Json(Environment.Travelers(This.Traveler_Manager_Index).Ticket));

						Message_Agent.Instance.Send(
							Destination_Address => Address,
							Object 				=> "message_handler",
							Service 			=> "traveler_enter_transfer",
							Params 				=> Parameters,
							Callback			=> null
						);

					end if;

			    end Get_Results;


				Parameters	: YAMI.Parameters.Parameters_Collection := YAMI.Parameters.Make_Parameters;
			begin

				Parameters.Set_String("station",Integer'Image(Next_Station));
				Parameters.Set_String("node_name",To_String(Next_Region));

				Put_Line(Environment.Get_Name_Server);

				Message_Agent.Instance.Send(
					Destination_Address => Environment.Get_Name_Server,
					Object 				=> "name_server",
					Service 			=> "get",
					Params 				=> Parameters,
					Callback			=> Get_Results'Access
				);

			exception
				when E : others =>
					Logger.Log(
		   				Sender => "Move_Operation",
		   				Message => "ERROR : Exception: " & Ada.Exceptions.Exception_Name(E) & "  " & Ada.Exceptions.Exception_Message(E),
		   				L => Logger.ERROR);
			end;
		end if;

	exception
		when Error : others =>
			Logger.Log(
				NAME_LEAVE,
				"Exception: " & Ada.Exceptions.Exception_Name(Error) & " , " & Ada.Exceptions.Exception_Message(Error),
				Logger.ERROR
			);
    end Do_Operation;

end Move_Operation;

