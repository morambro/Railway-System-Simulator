with Ada.Text_IO;use Ada.Text_IO;
with Environment;
with Trains;
with Route;
with Routes;

package body Train_Pool is

	task body Train_Type is

		Current_Descriptor 	: Train_Descriptor;
		Max_Speed 			: Integer;
		Next_Station 		: Positive;
		Next_Plattform 		: Positive;
		Next_Track			: Positive;

	begin
		loop
			Put_Line ("Train waits for a Descriptor");

			Trains_Queue.Dequeue(Current_Descriptor);

			Put_Line ("Got a descriptor");
			Max_Speed := Current_Descriptor.Speed;

			Next_Station 	:= Route.GetNextStation(Routes.Route(Current_Descriptor.Next_Stage));

	    	Next_Plattform 	:= Route.GetNextPlattform(Routes.Route(Current_Descriptor.Next_Stage));

	    	-- Train enters Next Station
			Environment.Stations(Next_Station).Enter(Current_Descriptor,Next_Plattform);

			Rand_Int.Reset(seed);

			Num := Rand_Int.Random(seed);

	    	Put_Line(
	      		"Train " & Integer'Image(Current_Descriptor.Id) &
		  		" Enters Plattform " & Integer'Image(Next_Plattform) &
	      		" At station " & Integer'Image(Next_Station));

			-- Update Current Station!!
			Current_Descriptor.Current_Station := Next_Station;

			delay Duration(Num);

			-- Train Leaves the station
	    	Environment.Stations(Next_Station).Leave(Current_Descriptor,Next_Plattform);


	   		Put_Line(
		      	"Train " & Integer'Image(Current_Descriptor.Id) &
			  	" Leaves Platform " & Integer'Image(Next_Plattform) &
			  	" At station " & Integer'Image(Next_Station));

			Next_Track := Route.GetNextTrack(Routes.Route(Current_Descriptor.Next_Stage));

			Environment.Tracks(Next_Track).Enter(Current_Descriptor,Max_Speed);

			Put_Line(
		      	"Train " & Integer'Image(Current_Descriptor.Id) &
			  	" Enters Track " & Integer'Image(Next_Track));
			Put_Line(
				"Train " & Integer'Image(Current_Descriptor.ID) & " running at speed " & Integer'Image(Max_Speed));
			Put_Line(
				"Train " & Integer'Image(Current_Descriptor.ID) & " Waiting for " & Rand_Range'Image(Num) & " seconds");

			Num := Rand_Int.Random(seed);
			delay Duration (Num);

			Environment.Tracks(Next_Track).Leave(Current_Descriptor);

			Put_Line(
		      	"Train " & Integer'Image(Current_Descriptor.Id) &
			  	" Leaves Platform " & Integer'Image(Next_Plattform) &
			  	" At station " & Integer'Image(Next_Station));

			Current_Descriptor.Next_Stage := Current_Descriptor.Next_Stage + 1;

			delay Duration (Num);

			Trains_Queue.Enqueue(Current_Descriptor);

		end loop;
	end Train_Type;

	procedure Associate(Train : Train_Descriptor) is
	begin
		Trains_Queue.Enqueue(Train);
	end Associate;

end Train_Pool;
