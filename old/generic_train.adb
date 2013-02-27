with Ada.Text_IO;use Ada.Text_IO;
with Environment;

package body Generic_Train is

	task body Train_Type is
		Current_Descriptor : Train_Descriptor;
		Max_Speed : Integer;
	begin
		select
			-- First Initialization

			Put_Line ("Got a descriptor");
			Max_Speed := Current_Descriptor.Speed;

			-- Put_Line("Train initialized with a descriptor");
			Put_Line("" & Integer'Image(Trains.Route(Current_Descriptor.Next_Stage).Next_Station));

			Rand_Int.Reset(seed);

			Num := Rand_Int.Random(seed);
--  			Put_Line(
--  				"Train " & Integer'Image(Current_Descriptor.ID) & " running at speed " & Integer'Image(Max_Speed));
--  			Put_Line(
--  				"Train " & Integer'Image(Current_Descriptor.ID) & " Waiting for " & Rand_Range'Image(Num) & " seconds");
--
--  			delay Duration(Num);

--  			Environment.Tracks(1).Leave(Current_Descriptor);

--  			Environment.Stations(1).Enter(Current_Descriptor,1);

			Put_Line("Train " & Integer'Image(Current_Descriptor.ID) &" Enters Plattform 1");

			Num := Rand_Int.Random(seed);
			delay Duration (Num);

--  			Environment.Stations(1).Leave(Current_Descriptor,1);

			Put_Line("Train " & Integer'Image(Current_Descriptor.ID) &" Leaved Plattform 1");

		end select;

	end Train_Type;

end Generic_Train;
