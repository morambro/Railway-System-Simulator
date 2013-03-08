with Logger;
with Environment;
with Tracks;
with Track;
with Trains;
with Route;
with Routes;
with Helper;
with Ada.Exceptions;  use Ada.Exceptions;

package body Train_Pool is


	-- #
	-- # Train task implementation
	-- #
	task body Train_Type is

		NAME : constant String := "Train_Pool.Train_Type";

		Current_Descriptor 	: Positive;
-- # 		Current_Descriptor 	: Train_Descriptor;
		Max_Speed 			: Integer;
		Next_Station 		: Positive;
		Next_Plattform 		: Positive;
		Next_Track			: Positive;
		Leg_Length 			: Positive;

		Time_In_Track 		: Float;

	begin
		loop begin

			-- # ####################### GAIN A DESCRIPTOR ###########################

			Logger.Log(NAME,"Train waits for a Descriptor",Logger.DEBUG);

			Trains_Queue.Dequeue(Current_Descriptor);

			Logger.Log(NAME,"Train task obtained a Descriptor",Logger.DEBUG);

			Max_Speed := Trains.Trains(Current_Descriptor).Speed;

			-- # ######################## NEXT TRACK ACCESS ############################

			-- # Retrieve next Track to travel
			Next_Track := Route.GetNextTrack(Routes.Route(Trains.Trains(Current_Descriptor).Next_Stage));


			-- # TODO : REMOVE DEBUG CODE!!!
			if ( Trains.Trains(Current_Descriptor).Id = 3333 ) then
				Trains.Trains(Current_Descriptor).Current_Station := 3;
			end if;

			Tracks.Tracks(Next_Track).Enter(Trains.Trains(Current_Descriptor),Max_Speed,Leg_Length);

			Logger.Log(
				NAME,
				"Train" & Integer'Image(Trains.Trains(Current_Descriptor).ID) & " entered the track",
				Logger.NOTICE
			);


			-- # Calculate Time to Travel the current track
			if(Trains.Trains(Current_Descriptor).Max_Speed < Max_Speed) then
				Trains.Trains(Current_Descriptor).Speed := Trains.Trains(Current_Descriptor).Max_Speed;
			else
				Trains.Trains(Current_Descriptor).Speed := Max_Speed;
			end if;

			Time_In_Track := 5.0;--Float(Leg_Length) / (Float(Current_Descriptor.Speed)*0.277777778);

			Logger.Log(NAME,
				"Train" & Integer'Image(Trains.Trains(Current_Descriptor).ID) & " running at speed "
				& Integer'Image(Trains.Trains(Current_Descriptor).Speed) & " km/h",
				Logger.NOTICE);

			Logger.Log(NAME,
				"Train" & Integer'Image(Trains.Trains(Current_Descriptor).ID) &
				" will run for " & Helper.Get_String(Time_In_Track,10) & " seconds",
				Logger.NOTICE);

			delay Duration (Time_In_Track);

			Tracks.Tracks(Next_Track).Leave(Trains.Trains(Current_Descriptor));


			-- # ######################## NEXT STATION ACCESS ############################

			-- # Retrieve Next station
			Next_Station 	:= Route.GetNextStation(Routes.Route(Trains.Trains(Current_Descriptor).Next_Stage));

			-- # Retrieve next platform number
	    	Next_Plattform 	:= Route.GetNextPlattform(Routes.Route(Trains.Trains(Current_Descriptor).Next_Stage));

	    	-- # Train enters Next Station
			--Environment.Stations(Next_Station).Enter(Current_Descriptor,Next_Plattform);


			Rand_Int.Reset(seed);

			Num := Rand_Int.Random(seed);

	    	Logger.Log(NAME,
	      		"Train" & Integer'Image(Trains.Trains(Current_Descriptor).Id) &
		  		" Enters Platform " & Integer'Image(Next_Plattform) &
	      		" At station " & Integer'Image(Next_Station), Logger.NOTICE);

			-- # Update Current Station!!
			Trains.Trains(Current_Descriptor).Current_Station := Next_Station;

			delay Duration(Num);

			-- # Train Leaves the station
	    	Environment.Stations(Next_Station).Leave(Trains.Trains(Current_Descriptor),Next_Plattform);


	   		Logger.Log(NAME,
		      	"Train" & Integer'Image(Trains.Trains(Current_Descriptor).Id) &
			  	" Leaves Platform " & Integer'Image(Next_Plattform) &
			  	" At station " & Integer'Image(Next_Station),Logger.NOTICE);


			Trains.Trains(Current_Descriptor).Next_Stage := Trains.Trains(Current_Descriptor).Next_Stage + 1;

			delay Duration (Num);

			-- # Re-enqueue the descriptor only if it has more stages to travel
			if(Trains.Trains(Current_Descriptor).Next_Stage < Routes.Route'Length) then
				Trains_Queue.Enqueue(Current_Descriptor);
			end if;

		-- # ############################ ERROR HANDLING ###############################
		exception

			-- # When the train track access results in a Bad_Track_Access_Request_Exception, the
			-- # current train descriptor is discarded.
			when E : Track.Bad_Track_Access_Request_Exception =>
				Logger.Log(
					NAME,
					"Train" & Integer'Image(Trains.Trains(Current_Descriptor).ID) &
					" Track access Error : " & Exception_Message(E),
					Logger.ERROR);
		end;

		end loop;
	end Train_Type;

	procedure Associate(Train : Positive) is
	begin
		Trains_Queue.Enqueue(Train);
	end Associate;

end Train_Pool;
