with Railway.Train;use Railway.Train;

package Railway.Track is
	
	
	-- Mantains unique ID of currently travelling trains
	type Train_Queue is array (Positive range <>) of Integer;
	
	protected type Track is
		
		entry Leave(T : Train_Descriptor);
		entry Enter(To_Add : Train_Descriptor);
	
	private 
	
		entry Retry(T : Train_Descriptor);
		Running_Trains : Train_Queue (1..10);
	
		Trains_Number : Integer := 0;
	
		Guard : Boolean := false;
		
		Retry_Num : Integer := 0;
	
	end Track;
	

end Railway.Track;
