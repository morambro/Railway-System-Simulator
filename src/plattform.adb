with Ada.Text_IO;use Ada.Text_IO;

package body Plattform is

	protected body Plattform_Type is
		entry Enter(Descriptor : in out Train_Descriptor) when Free = True is
		begin 
			Free := False;
		end Enter;
		
		procedure Leave(Descriptor : in out Train_Descriptor) is
		begin 
			Free := True;
		end Leave;
		
		
		procedure AddIncomingTraveler(Traveler : in out Traveler_Manager) is
		begin
			Arrival_Queue.Push(Traveler);
		end AddIncomingTraveler;
		
		
		procedure AddOutgoingTraveler(Traveler : in out Traveler_Manager) is
		begin
			Arrival_Queue.Push(Traveler);
			Put_Line("Travelers in queue = " & Integer'Image(Arrival_Queue.GetSize));
			
		end AddOutgoingTraveler;
		
	end Plattform_Type;
	
end Plattform;
