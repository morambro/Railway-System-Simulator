with Ada.Text_IO;use Ada.Text_IO;
with Ada.Strings.Unbounded;

package body Move_Operation is

	use Ada.Strings.Unbounded;
	
	procedure Do_Operation(This : in Move_Operation_Type) is
	begin
		Put_Line("Move Operation for Passenger " & To_String(This.Manager.Traveler.Name));
	end Do_Operation;

end Move_Operation;

