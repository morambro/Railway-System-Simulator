with Ada.Text_IO;use Ada.Text_IO;
with Ada.Containers;use Ada.Containers;


package body Platform is

	protected body Platform_Type is
		entry Enter(T : Train_Descriptor) when Free = True is
		begin
			Free := False;
		end Enter;

		procedure Leave(Descriptor : in out Train_Descriptor) is
		begin
			Free := True;
		end Leave;


		procedure AddIncomingTraveler(Traveler : in out Traveler_Manager) is
		begin
			Arrival_Queue.Enqueue(Traveler);
		end AddIncomingTraveler;


		procedure AddOutgoingTraveler(Traveler : in out Traveler_Manager) is
		begin
			Arrival_Queue.Enqueue(Traveler);
			Put_Line("Travelers in queue = " & Count_Type'Image(Arrival_Queue.Current_Use));

		end AddOutgoingTraveler;

	end Platform_Type;

end Platform;
