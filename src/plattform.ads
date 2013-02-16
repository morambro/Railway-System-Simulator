with Railway.Train;use Railway.Train;
with Queue;
with Traveler;use Traveler;

package Plattform is
	
	-- Create a queue for Traveler type
	package Traveler_Queue_Package is new Queue(Traveler_Manager);

	use Traveler_Queue_Package;

	-- Queue for Arriving Traveler
	Arrival_Queue : Queue_Type;
	
	-- Queue for Travelers waiting for the train
	Leaving_Queue : Queue_Type;

	protected type Plattform_Type(ID:Integer) is
		
		entry Enter(Descriptor : in out Train_Descriptor);
		
		procedure Leave(Descriptor : in out Train_Descriptor);
		
	private 
		Free : Boolean := True;
	end Plattform_Type;

end Plattform;
