with Logger;
with Environment;
with Trains;
with Route;
with Routes;

package body Train_Pool is

	task body Train_Type is

		NAME : constant String := "Train_Type";

		Current_Descriptor 	: Train_Descriptor;
		Max_Speed 			: Integer;
		Next_Station 		: Positive;
		Next_Plattform 		: Positive;
		Next_Track			: Positive;

	begin
		loop

			Logger.Log(NAME,"Train waits for a Descriptor",Logger.VERY_VERBOSE);

			Trains_Queue.Dequeue(Current_Descriptor);

			Logger.Log(NAME,"Train task obtained a Descriptor",Logger.VERY_VERBOSE);

			Max_Speed := Current_Descriptor.Speed;

			Next_Station 	:= Route.GetNextStation(Routes.Route(Current_Descriptor.Next_Stage));

	    	Next_Plattform 	:= Route.GetNextPlattform(Routes.Route(Current_Descriptor.Next_Stage));

	    	-- Train enters Next Station
			Environment.Stations(Next_Station).Enter(Current_Descriptor,Next_Plattform);

			Rand_Int.Reset(seed);

			Num := Rand_Int.Random(seed);

	    	Logger.Log(NAME,
	      		"Train " & Integer'Image(Current_Descriptor.Id) &
		  		" Enters Platform " & Integer'Image(Next_Plattform) &
	      		" At station " & Integer'Image(Next_Station), Logger.VERY_VERBOSE);

			-- Update Current Station!!
			Current_Descriptor.Current_Station := Next_Station;

			delay Duration(Num);

			-- Train Leaves the station
	    	Environment.Stations(Next_Station).Leave(Current_Descriptor,Next_Plattform);


	   		Logger.Log(NAME,
		      	"Train " & Integer'Image(Current_Descriptor.Id) &
			  	" Leaves Platform " & Integer'Image(Next_Plattform) &
			  	" At station " & Integer'Image(Next_Station),Logger.VERY_VERBOSE);

			Next_Track := Route.GetNextTrack(Routes.Route(Current_Descriptor.Next_Stage));

			Environment.Tracks(Next_Track).Enter(Current_Descriptor,Max_Speed);

			Logger.Log(NAME,
		      	"Train " & Integer'Image(Current_Descriptor.Id) &
			  	" Enters Track " & Integer'Image(Next_Track),Logger.VERY_VERBOSE);

			Logger.Log(NAME,
				"Train " & Integer'Image(Current_Descriptor.ID) & " running at speed " & Integer'Image(Max_Speed),
				Logger.VERY_VERBOSE);

			Logger.Log(NAME,
				"Train " & Integer'Image(Current_Descriptor.ID) & " Waiting for " & Rand_Range'Image(Num) & " seconds",
				Logger.VERY_VERBOSE);

			Num := Rand_Int.Random(seed);
			delay Duration (Num);

			Environment.Tracks(Next_Track).Leave(Current_Descriptor);

			Logger.Log(NAME,
		      	"Train " & Integer'Image(Current_Descriptor.Id) &
			  	" Leaves Platform " & Integer'Image(Next_Plattform) &
			  	" At station " & Integer'Image(Next_Station),
			  	Logger.VERY_VERBOSE);

			Current_Descriptor.Next_Stage := Current_Descriptor.Next_Stage + 1;

			delay Duration (Num);

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
