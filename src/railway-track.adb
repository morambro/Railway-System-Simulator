with Ada.Text_IO;use Ada.Text_IO;

package body Railway.Track is
	
	protected body Track is 
		
		entry Leave(T : Train_Descriptor) when true is
		begin
			if(Running_Trains(1) = T.ID) then
				for I in Integer range 2 .. 10 loop
					Running_Trains(I-1) := Running_Trains(I);
				end loop;
				if(Retry'Count > 0) then
					Retry_Num := Retry'Count;				
					Guard := true;
				end if;
				Put_Line("Train " & Integer'Image(T.ID) & " Leaves!");
				Trains_Number := Trains_Number - 1;
			else 
				Put_Line("Train " & Integer'Image(T.ID) & " Cannot leave because it's not the first"); 
				requeue Retry;
			end if;
		end Leave;
		
		entry Enter(To_Add : in out Train_Descriptor; Max_Speed : out Integer) when true is
		begin
			if(Trains_Number < 10) then
				Trains_Number := Trains_Number + 1;
				Running_Trains(Trains_Number) := To_Add.ID;
				Put_Line("Train " & Integer'Image(To_Add.ID) & " Added to Position : " & Integer'Image(Trains_Number));
				if(To_Add.Max_Speed > Curr_Max_Speed) then
					Max_Speed := Curr_Max_Speed;
				else 
					Curr_Max_Speed := To_Add.Max_Speed;
					Max_Speed := To_Add.Max_Speed;
				end if;
			end if;
		end Enter;
		
		entry Retry(T : Train_Descriptor) when Guard = true is
		begin
			
			Retry_Num := Retry_Num - 1;
			if(Retry_Num = 0) then
				Guard := false;
			end if;
			
			if(Running_Trains(1) = T.ID) then
				for I in Integer range 2 .. 10 loop
					Running_Trains(I-1) := Running_Trains(I);
				end loop;
				if(Retry'Count > 0) then
					Retry_Num := Retry'Count;				
					Guard := true;
				end if;
				Put_Line("Train " & Integer'Image(T.ID) & " Leaves!");
				Trains_Number := Trains_Number - 1;
			else 
				Put_Line("Train " & Integer'Image(T.ID) & " Cannot leave because it's not the first"); 
				requeue Retry;
			end if;
		end Retry;
	
	end Track;
		
	
end Railway.Track;
