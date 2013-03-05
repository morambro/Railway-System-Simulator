with Logger;
with Environment;
with Trains;
with Route;
with Routes;
with Helper;

package body Train_Pool is

	task body Train_Type is

		NAME : constant String := "Train_Pool.Train_Type";

		Current_Descriptor 	: Train_Descriptor;
		Max_Speed 			: Integer;
		Next_Station 		: Positive;
		Next_Plattform 		: Positive;
		Next_Track			: Positive;
		Leg_Length 			: Positive;

		Time_In_Track 		: Float;

	begin
		loop

			Logger.Log(NAME,"Train waits for a Descriptor",Logger.DEBUG);

			Trains_Queue.Dequeue(Current_Descriptor);

			Logger.Log(NAME,"Train task obtained a Descriptor",Logger.DEBUG);

			Max_Speed := Current_Descriptor.Speed;

			-- Retrieve Next station
			Next_Station 	:= Route.GetNextStation(Routes.Route(Current_Descriptor.Next_Stage));

			-- Retrieve next platform number
	    	Next_Plattform 	:= Route.GetNextPlattform(Routes.Route(Current_Descriptor.Next_Stage));

	    	-- Train enters Next Station
			Environment.Stations(Next_Station).Enter(Current_Descriptor,Next_Plattform);


			Rand_Int.Reset(seed);

			Num := Rand_Int.Random(seed);

	    	Logger.Log(NAME,
	      		"Train " & Integer'Image(Current_Descriptor.Id) &
		  		" Enters Platform " & Integer'Image(Next_Plattform) &
	      		" At station " & Integer'Image(Next_Station), Logger.NOTICE);

			-- Update Current Station!!
			Current_Descriptor.Current_Station := Next_Station;

			delay Duration(Num);

			-- Train Leaves the station
	    	Environment.Stations(Next_Station).Leave(Current_Descriptor,Next_Plattform);


	   		Logger.Log(NAME,
		      	"Train " & Integer'Image(Current_Descriptor.Id) &
			  	" Leaves Platform " & Integer'Image(Next_Plattform) &
			  	" At station " & Integer'Image(Next_Station),Logger.NOTICE);

			-- Retrieve next Track to travel
			Next_Track := Route.GetNextTrack(Routes.Route(Current_Descriptor.Next_Stage));


			if ( Current_Descriptor.Id = 3333 ) then
				Current_Descriptor.Current_Station := 3;
			end if;


			Environment.Tracks(Next_Track).Enter(Current_Descriptor,Max_Speed,Leg_Length);


			-- Time to Travel Calculus
			if(Current_Descriptor.Max_Speed < Max_Speed) then
				Current_Descriptor.Speed := Current_Descriptor.Max_Speed;
			else
				Current_Descriptor.Speed := Max_Speed;
			end if;

			Time_In_Track := Float(Leg_Length) / Float(Current_Descriptor.Speed) * 60.0;


--  			Logger.Log(NAME,
--  		      	"Train " & Integer'Image(Current_Descriptor.Id) &
--  			  	" Enters Track " & Integer'Image(Next_Track),Logger.NOTICE);

			Logger.Log(NAME,
				"Train " & Integer'Image(Current_Descriptor.ID) & " running at speed "
				& Integer'Image(Current_Descriptor.Speed) & " km/h",
				Logger.NOTICE);

			Logger.Log(NAME,
				"Train " & Integer'Image(Current_Descriptor.ID) &
				" will run for " & Helper.Get_String(Time_In_Track,10) & " seconds",
				Logger.NOTICE);

			delay Duration (Time_In_Track);

			Environment.Tracks(Next_Track).Leave(Current_Descriptor);

--  			Logger.Log(NAME,
--  		      	"Train " & Integer'Image(Current_Descriptor.Id) &
--  			  	" Leaves Track ",
--  			  	Logger.NOTICE);

			Current_Descriptor.Next_Stage := Current_Descriptor.Next_Stage + 1;

			delay Duration (Num);

			-- Re-enqueue the descriptor only if it has more stages to travel
			if(Current_Descriptor.Next_Stage < Routes.Route'Length) then
				Trains_Queue.Enqueue(Current_Descriptor);
			end if;

		end loop;
	end Train_Type;

	procedure Associate(Train : Train_Descriptor) is
	begin
		Trains_Queue.Enqueue(Train);
	end Associate;

end Train_Pool;
