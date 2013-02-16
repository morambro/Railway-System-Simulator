with Generic_Operation_Interface;use Generic_Operation_Interface;
with Traveler; use Traveler;
 
package Move_Operation is

	type Move_Operation_Type(Manager : access Traveler_Manager) is new Operation_Interface with private;

	overriding procedure Do_Operation(This : in Move_Operation_Type);
	
private
	
	type Move_Operation_Type(Manager : access Traveler_Manager) is new Operation_Interface with record
		null;
	end record;

end Move_Operation;
