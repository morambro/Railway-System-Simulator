with Generic_Operation_Interface;use Generic_Operation_Interface;
with Traveler; use Traveler;

package Move_Operation is

	type Move_Operation_Type(Manager : access Traveler_Manager) is new Operation_Interface with null record;

	overriding procedure Do_Operation(This : in Move_Operation_Type);
	
private
	
	N : Positive := 1;

end Move_Operation;
