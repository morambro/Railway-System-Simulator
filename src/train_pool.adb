with Ada.Text_IO;use Ada.Text_IO;
with Environment;


package body Train_Pool is
	
	task body Train_Type is 
	
		Current_Descriptor : Train_Descriptor;
		Max_Speed : Integer;
	begin
		loop
			Put_Line ("Train waits for a Descriptor");

			Trains_Queue.Dequeue(Current_Descriptor);		
					
			Put_Line ("Got a descriptor");
			Max_Speed := Current_Descriptor.Speed;

			-- Put_Line("Train initialized with a descriptor");
			Environment.Tracks(1).Enter(Current_Descriptor,Max_Speed);

			Rand_Int.Reset(seed);

			Num := Rand_Int.Random(seed);
			Put_Line(
				"Train " & Integer'Image(Current_Descriptor.ID) & " running at speed " & Integer'Image(Max_Speed));
			Put_Line(
				"Train " & Integer'Image(Current_Descriptor.ID) & " Waiting for " & Rand_Range'Image(Num) & " seconds");

			delay Duration(Num);

			Environment.Tracks(1).Leave(Current_Descriptor);	
		
			Environment.Stations(1).Enter(Current_Descriptor,1);
		
			Put_Line("Train " & Integer'Image(Current_Descriptor.ID) &" Enters Plattform 1");

			Num := Rand_Int.Random(seed);
			delay Duration (Num);
		
			Environment.Stations(1).Leave(Current_Descriptor,1);
		
			Put_Line("Train " & Integer'Image(Current_Descriptor.ID) &" Leaves Plattform 1");
		end loop;	
	end Train_Type;
	
	procedure Associate(Train : Train_Descriptor) is 
	begin
		Trains_Queue.Enqueue(Train);
	end Associate;
	
end Train_Pool;
