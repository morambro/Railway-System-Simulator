with Generic_Operation_Interface; use Generic_Operation_Interface;
with Passenger;use Passenger;

package Passenger.Move_Operation is

	type Move_Operation_Type(Passenger : access Passenger_Type) is new Operation_Interface with private;

	overriding procedure Do_Operation(This : in Move_Operation_Type);
	
private
	
	type Move_Operation_Type(Passenger : access Passenger_Type) is new Operation_Interface with record
		null;
	end record;

end Passenger.Move_Operation;
