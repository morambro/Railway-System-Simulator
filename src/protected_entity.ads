package Protected_Entity is

	protected Synch is
	
		entry Wait;
		
		procedure Open;
		
	end Synch;
	
	Continue : Boolean := False;

end Protected_Entity;
		
