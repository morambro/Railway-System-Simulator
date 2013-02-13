with Track_Container;
with Ada.Text_IO;use Ada.Text_IO;

package body Railway.Train is
	
	task body Train_Type is
		Current_Descriptor : Train_Descriptor;
		Max_Speed : Integer;
	begin
		loop
			select 
				accept Initialize(Descr : Train_Descriptor) do
					Current_Descriptor := Descr;
				end;
			or
			
				Max_Speed := Current_Descriptor.Speed;
			
				-- Put_Line("Train initialized with a descriptor");
				Track_Container.Track_1.Enter(Current_Descriptor,Max_Speed);
			
				Rand_Int.Reset(seed);
			
				Num := Rand_Int.Random(seed);
				Put_Line("Train " & Integer'Image(Current_Descriptor.ID) & " running at speed " & Integer'Image(Max_Speed));
				Put_Line("Train " & Integer'Image(Current_Descriptor.ID) &" Waiting for " & Rand_Range'Image(Num) & " seconds");
			
				delay Duration (Num);
			
				Track_Container.Track_1.Leave(Current_Descriptor);	
		
			end select;
		end loop;
		
	end Train_Type;
	
end Railway.Train;
