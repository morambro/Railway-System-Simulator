with Generic_Operation_Interface;use Generic_Operation_Interface;
with Traveler; use Traveler;

package Move_Operation is

	type Move_Operation_Type is new Operation_Interface with record
		Manager : access Traveler_Manager := null;
	end record;

	overriding procedure Do_Operation(This : in Move_Operation_Type);
	
	function NewOperation(T_Manager : access Traveler_Manager) return Any_Operation;  
	
private

	
	
	
	N : Positive := 1;

end Move_Operation;
