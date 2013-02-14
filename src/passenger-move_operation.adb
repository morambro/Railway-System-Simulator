with Ada.Text_IO;use Ada.Text_IO;

package body Passenger.Move_Operation is

	procedure Do_Operation(This : in Move_Operation_Type) is
	begin
		Put_Line("Move Operation for Passenger " & This.Passenger.GetName );
	end Do_Operation;

end Passenger.Move_Operation;

