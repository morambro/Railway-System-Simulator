with Ada.Text_IO;use Ada.Text_IO;
with Ada.Strings.Unbounded;
with Environment;

package body Move_Operation is

	use Ada.Strings.Unbounded;

	procedure Do_Operation(This : in Move_Operation_Type) is
	begin

		Put_Line("Move Operation for Passenger " & To_String(This.Manager.Traveler.Name));

		Environment.Stations(1).WaitForTrain(This.Manager.all,2);

		-- Points to the next Operation to
		This.Manager.Next_Operation := This.Manager.Next_Operation + 1;
		N := N + 1;
		Put_Line("Added to queue");
	end Do_Operation;

end Move_Operation;

