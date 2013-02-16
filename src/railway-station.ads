with Railway.Train;use Railway.Train;

package Railway.Station is

	---------------------------------- STATION INTERFACE --------------------------------------
	type Station_Interface is interface;

		-- Station_Interface Methods
		procedure Enter(Station : Station_Interface ; Descriptor : in out Train_Descriptor;Plattform : Integer) 
		is abstract;
	
		procedure Leave(Station : Station_Interface ; Descriptor : in out Train_Descriptor;Plattform : Integer) 
		is abstract;

	-- End Of the Interface
	
   	-- generic Station reference type to be used inside records: Type'Class doesn't
   	-- have a fixed size so it can be not allocated inside a record.
   	type Station_Ref is access all Station_Interface'Class;

	------------------------------- PLATTFORM TYPE ----------------------------------------------
	protected type Plattform(ID:Integer) is
		entry Enter(Descriptor : in out Train_Descriptor);
		procedure Leave(Descriptor : in out Train_Descriptor);
	private 
		Free : Boolean := True;
	end Plattform;
	
	
end Railway.Station;
