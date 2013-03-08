--==============================================================================
-- File:
--	main.adb
-- Author:
--	Moreno Ambrosin
--	Mat.  : 1035635
-- Date:
-- 	09/02/2013
--==============================================================================

with Ada.Command_Line;
with Ada.Exceptions;
with Ada.Text_IO;


with Environment;use Environment;
with Trains;
with Route;use Route;
with Train_Pool;
With Task_Pool;

with Track;

with Generic_Station;

with Logger;

with Traveler;

with Message_Agent;use Message_Agent;

procedure Main is

begin

	if Ada.Command_Line.Argument_Count < 2 then
		Ada.Text_IO.Put_Line("Expecting a log level [ -i | -n | -d ]. And a tcp address in the form tcp://...");
		Ada.Command_Line.Set_Exit_Status(Ada.Command_Line.Failure);
		return;
	end if;

	if(not Logger.Init(Ada.Command_Line.Argument (1))) then
		Ada.Text_IO.Put_Line("Wrong parameter! Use a valid log level [ -i | -n | -d ].");
		return;
	end if;

	declare
		-- Creation of Actors for Travelers
		Traveler_Tasks 	: Task_Pool.Task_Pool_Type(5);
		Pool 			: Train_Pool.Train_Task_Pool(3);
		Node_Address 	: String := Ada.Command_Line.Argument (2);

		Client : access Message_Agent_Type := new Message_Agent_Type;

	begin

--  		Train_Pool.Associate(1);
--  		Train_Pool.Associate(2);
--  		Train_Pool.Associate(3);
--  		Train_Pool.Associate(4);

		Client.Send(Dest => Node_Address,Message => "Prova");

		null;
	end;

exception
	when E : others => Ada.Text_IO.Put_Line(Ada.Exceptions.Exception_Message (E));
end Main;
