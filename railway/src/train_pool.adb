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
with Segments;
with Segment;
with Trains;
with Route;
with Routes;
with Helper;
with Ada.Exceptions;  use Ada.Exceptions;
with Ada.Text_IO;use Ada.Text_IO;
with Train;
with Gateway_Platform;
with Ada	.Calendar;
with Time_Table;

package body Train_Pool is


	-- #
	-- # Train task implementation
	-- #
	task body Train_Executor_Task is

		NAME : constant String := "Train_Pool.Train_Type";

		Current_Descriptor_Index 	: Positive;
		Terminated 					: Boolean;

	begin
		MAIN_LOOP:
		loop begin
			-- # ####################### GAIN A DESCRIPTOR ###########################

			Logger.Log(NAME,"Train waits for a Descriptor",Logger.DEBUG);

			if Priority_Level = HIGH then

				High_Priority_Trains_Queue.Dequeue(
					To_Get 		=> Current_Descriptor_Index,
					Terminated 	=> Terminated
				);
			else
				Low_Priority_Trains_Queue.Dequeue(
					To_Get 		=> Current_Descriptor_Index,
					Terminated 	=> Terminated
				);
			end if;


			exit MAIN_LOOP when Terminated;

			Logger.Log(NAME,"Train task obtained a Descriptor",Logger.DEBUG);

			declare
				Max_Speed 			: Integer  := Trains.Trains(Current_Descriptor_Index).Speed;
					-- # Retrieve Next station
				Route_Index 		: Positive := Trains.Trains(Current_Descriptor_Index).Route_Index;

				Next_Stage 			: Positive := Trains.Trains(Current_Descriptor_Index).Next_Stage;

				-- # Retrieve start Station from which start
				Start_Station 	    : Positive := Routes.All_Routes(Route_Index)(Next_Stage).Start_Station;

				-- # Retrieve start Platfrom from which start
				Start_Platform 	    : Positive := Routes.All_Routes(Route_Index)(Next_Stage).Start_Platform;

				-- # Retrieve next Segment to travel
				Next_Station 	    : Positive := Routes.All_Routes(Route_Index)(Next_Stage).Next_Station;
				-- # Retrieve next platform number
		    	Next_Platform 		: Positive := Routes.All_Routes(Route_Index)(Next_Stage).Platform_Index;

				Next_Segment		: Positive := Routes.All_Routes(Route_Index)(Next_Stage).Next_Segment;

				Leg_Length 			: Positive;

				Time_In_Segment 	: Float;

			-- # ######################## NEXT Segment ACCESS ############################
			begin

				Logger.Log(
					NAME,
					"Train" & Integer'Image(Trains.Trains(Current_Descriptor_Index).ID) &
					" leaves Station " & Integer'Image(Start_Station) & ", platform " & Integer'Image(Start_Platform),
					Logger.DEBUG
				);

				-- # Update Current Station!!
				Trains.Trains(Current_Descriptor_Index).Current_Station := Start_Station;

				Put_Line("NEXT_STAGE = " & Integer'Image(Next_Stage));

				-- # Wait Until time to leave
				declare
					Current_Array_Index 	: Positive :=
						Environment.T(Trains.Trains(Current_Descriptor_Index).Route_Index).Current_Array_Index;

					Current_Array_Position	: Positive :=
						Environment.T(Trains.Trains(Current_Descriptor_Index).Route_Index).Current_Array_Position;

					Time_To_Wait : Ada.Calendar.Time := Environment.T(Trains.Trains(Current_Descriptor_Index).Route_Index).Table
						(Current_Array_Index)(Current_Array_Position);
				begin
					Logger.Log(
						NAME,
						"Train" & Integer'Image(Trains.Trains(Current_Descriptor_Index).ID) &
						" wait until " & Time_Table.Get_Time_Representation(Time_To_Wait),
						Logger.DEBUG
					);
					delay until Time_To_Wait;

					Time_Table.Update_Time_Table(Environment.T(Trains.Trains(Current_Descriptor_Index).Route_Index));

				end;

				-- # Train Leaves the station
		    	Environment.Stations(Start_Station).Leave(
		    		Descriptor_Index 	=> Current_Descriptor_Index,
		    		Platform_Index		=> Start_Platform,
		    		Action				=> Routes.All_Routes(Route_Index)(Next_Stage).Leave_Action);

				Segments.Segments(Next_Segment).Enter(Current_Descriptor_Index,Max_Speed,Leg_Length);

				Logger.Log(
					NAME,
					"Train" & Integer'Image(Trains.Trains(Current_Descriptor_Index).ID) &
					" entered Segment " & Integer'Image(Next_Segment),
					Logger.DEBUG
				);

				-- # Calculate Time to Travel the current Segment
				if(Trains.Trains(Current_Descriptor_Index).Max_Speed < Max_Speed) then
					Trains.Trains(Current_Descriptor_Index).Speed := Trains.Trains(Current_Descriptor_Index).Max_Speed;
				else
					Trains.Trains(Current_Descriptor_Index).Speed := Max_Speed;
				end if;

				-- # The time to spent running inside the segment is simply calculated by the quotient
				-- # between the length of the Segment and the Speed of the train; the first is
				-- # expressed in [unit], the second in [unit]/[sec]
				Time_In_Segment := Float(Leg_Length)/Float(Trains.Trains(Current_Descriptor_Index).Speed);

				Logger.Log(NAME,
					"Train" & Integer'Image(Trains.Trains(Current_Descriptor_Index).ID) & " running at speed "
					& Integer'Image(Trains.Trains(Current_Descriptor_Index).Speed) & " u/s;"
					& " will wait for " & Helper.Get_String(Time_In_Segment,10) & " seconds",
					Logger.DEBUG);

				delay Duration (Time_In_Segment);

				Segments.Segments(Next_Segment).Leave(Current_Descriptor_Index);

				Logger.Log(NAME,
		      		"Train" & Integer'Image(Trains.Trains(Current_Descriptor_Index).Id) &
		      		" leaved Segment " & Integer'Image(Next_Segment),
		      		Logger.DEBUG);

				-- # ######################## NEXT STATION ACCESS ############################

				Put_Line("NEXT_STAGESSSSSSS = " & Integer'Image(Trains.Trains(Current_Descriptor_Index).Next_Stage));

				Put_Line("ROUTE_LENGTH = " & Integer'Image(Routes.All_Routes(Route_Index)'Length));

		    	-- # Train enters Next Station
				Environment.Stations(Next_Station).Enter(
					Descriptor_Index	=> Current_Descriptor_Index,
					Platform_Index		=> Next_Platform,
					Segment_ID 			=> Segments.Segments(Next_Segment).Id,
					Action				=> Routes.All_Routes(Route_Index)(Next_Stage).Enter_Action
				);

				-- # Go to the next Stage!
				Trains.Trains(Current_Descriptor_Index).Next_Stage := Trains.Trains(Current_Descriptor_Index).Next_Stage + 1;

				Rand_Int.Reset(seed);

				Num := Rand_Int.Random(seed);

				-- # Re-enqueue the descriptor only if it has more stages to travel
				if(Trains.Trains(Current_Descriptor_Index).Next_Stage <= Routes.All_Routes(Route_Index)'Length) then
					Associate(Current_Descriptor_Index);
				else
					Logger.Log(NAME,
				      	"Train" & Integer'Image(Trains.Trains(Current_Descriptor_Index).Id) &
					  	" finished its run!",Logger.DEBUG);
				end if;

			-- # ############################ ERROR HANDLING ###############################
			end;
		exception

			-- # When the train Segment access results in a Bad_Segment_Access_Request_Exception, the
			-- # current train descriptor is discarded.
			when E : Segment.Bad_Segment_Access_Request_Exception =>
				Logger.Log(
					NAME,
					"Train" & Integer'Image(Trains.Trains(Current_Descriptor_Index).ID) &
					" Segment access Error : " & Exception_Message(E),
					Logger.ERROR);
			when Ex : Gateway_Platform.Stop_Train_Execution =>
				Logger.Log(
					NAME,
					"Train" & Integer'Image(Trains.Trains(Current_Descriptor_Index).ID) &
					" Interruption : " & Exception_Message(Ex),
					Logger.ERROR);
			when Exc : others =>
				Put_Line("ERROR : Exception: " & Ada.Exceptions.Exception_Name(Exc) & "  " & Ada.Exceptions.Exception_Message(Exc));
		end;
		end loop MAIN_LOOP;

		Logger.Log(
			NAME,
			"Task Received Termination Signal. Bye!",
			Logger.DEBUG);

	end Train_Executor_Task;


	procedure Associate(Train_D : Positive) is
	begin
		-- # Put the Train Descriptor index in the right queue, depending on his
		-- # type.
		if Trains.Trains(Train_D).T_Type = Train.FB then
			High_Priority_Trains_Queue.Enqueue(Train_D);
		else
			Low_Priority_Trains_Queue.Enqueue(Train_D);
		end if;
	end Associate;


	procedure Stop is
	begin
		Low_Priority_Trains_Queue.Stop;
	end Stop;

end Train_Pool;
