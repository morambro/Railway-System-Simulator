package body Protected_Entity is

	protected body Synch is
	
		entry Wait when Continue = True is
		begin
			null;
		end Wait;
		
		procedure Open is
		begin
			Continue := True;
		end;
		
	end Synch;

end Protected_Entity;
