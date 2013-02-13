with Railway.Train;use Railway.Train;

package Railway.Track is
	
	
	-- Mantains unique ID of currently travelling trains
	type Train_Queue is array (Positive range <>) of Integer;
	
	protected type Track is
		
		entry Leave(T : Train_Descriptor);
		entry Enter(To_Add :  in out Train_Descriptor; Max_Speed : out Integer);
	
	private 
	
		entry Retry(T : Train_Descriptor);
		Running_Trains : Train_Queue (1..10);
	
		Trains_Number : Integer := 0;
	
		Guard : Boolean := false;
		
		Retry_Num : Integer := 0;
		
		Curr_Max_Speed : Integer := 200; 
	
	end Track;
	

end Railway.Track;
