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
with Tracks;
with Track;
with Trains;
with Route;
with Routes;
with Helper;
with Ada.Exceptions;  use Ada.Exceptions;
with Ada.Text_IO;
with Train;

package body Train_Pool is


	-- #
	-- # Train task implementation
	-- #
	task body Low_Priority_Train_Type is

		NAME : constant String := "Train_Pool.Train_Type";

		Current_Descriptor_Index 	: Positive;

	begin
		MAIN_LOOP:
		loop begin
			-- # ####################### GAIN A DESCRIPTOR ###########################

			select
				accept Stop;
				exit MAIN_LOOP;
			else
				null;
			end select;

			Logger.Log(NAME,"Train waits for a Descriptor",Logger.DEBUG);

			Low_Priority_Trains_Queue.Dequeue(Current_Descriptor_Index);

			Logger.Log(NAME,"Train task obtained a Descriptor",Logger.DEBUG);

			declare
				Max_Speed 			: Integer  := Trains.Trains(Current_Descriptor_Index).Speed;
					-- # Retrieve Next station
				Route_Index 		: Positive := Trains.Trains(Current_Descriptor_Index).Route_Index;
				Next_Stage 			: Positive := Trains.Trains(Current_Descriptor_Index).Next_Stage;
				-- # Retrieve next Track to travel
				Next_Station 	    : Positive := Route.Get_Next_Station(Routes.All_Routes(Route_Index)(Next_Stage));
				-- # Retrieve next platform number
		    	Next_Platform 		: Positive := Route.Get_Next_Platform(Routes.All_Routes(Route_Index)(Next_Stage));
				Next_Track			: Positive := Route.Get_Next_Track(Routes.All_Routes(Route_Index)(Next_Stage));
				Leg_Length 			: Positive;

				Time_In_Track 		: Float;

			-- # ######################## NEXT TRACK ACCESS ############################
			begin
	--  			-- # TODO : REMOVE DEBUG CODE!!!
	--  			if ( Trains.Trains(Current_Descriptor_Index).Id = 3333 ) then
	--  				Trains.Trains(Current_Descriptor_Index).Current_Station := 3;
	--  			end if;

				Tracks.Tracks(Next_Track).Enter(Current_Descriptor_Index,Max_Speed,Leg_Length);

				Logger.Log(
					NAME,
					"Train" & Integer'Image(Trains.Trains(Current_Descriptor_Index).ID) &
					" entered Track Number " & Integer'Image(Next_Track),
					Logger.NOTICE
				);

				-- # Calculate Time to Travel the current track
				if(Trains.Trains(Current_Descriptor_Index).Max_Speed < Max_Speed) then
					Trains.Trains(Current_Descriptor_Index).Speed := Trains.Trains(Current_Descriptor_Index).Max_Speed;
				else
					Trains.Trains(Current_Descriptor_Index).Speed := Max_Speed;
				end if;

				Time_In_Track := 3.0;--Float(Leg_Length) / (Float(Current_Descriptor_Index.Speed)*0.277777778);

				Logger.Log(NAME,
					"Train" & Integer'Image(Trains.Trains(Current_Descriptor_Index).ID) & " running at speed "
					& Integer'Image(Trains.Trains(Current_Descriptor_Index).Speed) & " km/h",
					Logger.NOTICE);

				Logger.Log(NAME,
					"Train" & Integer'Image(Trains.Trains(Current_Descriptor_Index).ID) &
					" will run for " & Helper.Get_String(Time_In_Track,10) & " seconds",
					Logger.NOTICE);

				delay Duration (Time_In_Track);

				Tracks.Tracks(Next_Track).Leave(Current_Descriptor_Index);


				-- # ######################## NEXT STATION ACCESS ############################

		    	-- # Train enters Next Station
				--Environment.Get_Regional_Stations(Next_Station).Enter(Current_Descriptor_Index,Next_Platform);

				Rand_Int.Reset(seed);

				Num := Rand_Int.Random(seed);

		    	Logger.Log(NAME,
		      		"Train" & Integer'Image(Trains.Trains(Current_Descriptor_Index).Id) &
		      		" leaved Track Number" & Integer'Image(Next_Track) &
			  		" entered Platform " & Integer'Image(Next_Platform) &
		      		" at Station " & Integer'Image(Next_Station), Logger.NOTICE);

				-- # Update Current Station!!
				Trains.Trains(Current_Descriptor_Index).Current_Station := Next_Station;

				delay Duration(Num);

				-- # Train Leaves the station
		    	Environment.Get_Regional_Stations(Next_Station).Leave(Current_Descriptor_Index,Next_Platform);


		   		Logger.Log(
		   			NAME,
			      	"Train" & Integer'Image(Trains.Trains(Current_Descriptor_Index).Id) &
				  	" leaved Platform " & Integer'Image(Next_Platform) &
				  	" at Station " & Integer'Image(Next_Station),
				  	Logger.NOTICE);


				Trains.Trains(Current_Descriptor_Index).Next_Stage := Trains.Trains(Current_Descriptor_Index).Next_Stage + 1;

				delay Duration (Num);

				-- # Re-enqueue the descriptor only if it has more stages to travel
				if(Trains.Trains(Current_Descriptor_Index).Next_Stage <= Routes.All_Routes(Route_Index)'Length) then
					Low_Priority_Trains_Queue.Enqueue(Current_Descriptor_Index);
				else
					Logger.Log(NAME,
				      	"Train" & Integer'Image(Trains.Trains(Current_Descriptor_Index).Id) &
					  	" finished its run!",Logger.NOTICE);
				end if;

			-- # ############################ ERROR HANDLING ###############################
			end;
		exception

			-- # When the train track access results in a Bad_Track_Access_Request_Exception, the
			-- # current train descriptor is discarded.
			when E : Track.Bad_Track_Access_Request_Exception =>
				Logger.Log(
					NAME,
					"Train" & Integer'Image(Trains.Trains(Current_Descriptor_Index).ID) &
					" Track access Error : " & Exception_Message(E),
					Logger.ERROR);
		end;
		end loop MAIN_LOOP;
	end Low_Priority_Train_Type;


	task body High_Priority_Train_Type is
	begin
		null;
    end High_Priority_Train_Type;

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

end Train_Pool;