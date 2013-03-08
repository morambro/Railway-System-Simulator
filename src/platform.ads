with Train;use Train;
with Queue;
with Traveler;use Traveler;

package Platform is

	-- Create a queue for Traveler type
	package Traveler_Queue_Package is new Queue(Element => Traveler_Manager);

	-- Queue for Arriving Traveler
	Arrival_Queue : Traveler_Queue_Package.Unbounded_Queue.Queue;

	-- Queue for Travelers waiting for the train
	Leaving_Queue : Traveler_Queue_Package.Unbounded_Queue.Queue;

	protected type Platform_Type(ID:Integer) is

		entry Enter(T : Train_Descriptor);

		procedure Leave(Descriptor : in out Train_Descriptor);

		procedure AddIncomingTraveler(Traveler : in out Traveler_Manager);

		procedure AddOutgoingTraveler(Traveler : in out Traveler_Manager);

	private
		Free : Boolean := True;
	end Platform_Type;

end Platform;
