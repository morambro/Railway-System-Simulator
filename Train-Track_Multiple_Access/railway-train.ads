package Railway.Train is
	
	type Train_Descriptor;
		
	type Train_Descriptor is
	record
		ID : Integer;	
	end record;	
	
	task type Train_Type(Descriptor : Integer) is 
	end Train_Type;
	
end Railway.Train;
