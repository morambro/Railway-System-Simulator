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
with Generic_Operation_Interface;

with Environment;use Environment;
with Trains;
with Route;use Route;
with Train_Pool;
With Task_Pool;

with Track;

with Generic_Station;

with Logger;

with Traveler;

with Message_Agent;

with YAMI.Outgoing_Messages; use YAMI.Outgoing_Messages;
with YAMI.Agents;
with YAMI.Parameters;

with Ada.Exceptions;  use Ada.Exceptions;

procedure Main is

	use Message_Agent;

	INSTRUCTIONS : constant String :=
	"------------------------------------------------------------------------" & ASCII.LF &
	"The following parameters must be specified:" & ASCII.LF &
  		"  1) Log level, choosen from [ -i | -n | -d ] \n" & ASCII.LF &
  		"  2) Name Server tcp Address [ -ns ] (e.g. -ns tcp://localhost:9000)" & ASCII.LF &
  		"  3) Node name identifier [ -nn ] (e.g. -nn Node1) " & ASCII.LF &
  		"  4) Node address [ -na ] (e.g. -na tcp://...)" & ASCII.LF &
  	"------------------------------------------------------------------------";

begin

	if Ada.Command_Line.Argument_Count < 7 then
		-- ./run -name Node_Name -address tcp_//... -ns_address tcp://name_server_address [ -i | -n | -d ]
		Ada.Text_IO.Put_Line("ERROR : Not enougth arguments!");
		Ada.Text_IO.Put_Line(INSTRUCTIONS);
		Ada.Command_Line.Set_Exit_Status(Ada.Command_Line.Failure);
		return;
	end if;

	declare

		Log_Level 	: constant String := Ada.Command_Line.Argument (1);
		Par_1 	  	: constant String := Ada.Command_Line.Argument (2);
		Name_Server : constant String := Ada.Command_Line.Argument (3);
		Par_2 	  	: constant String := Ada.Command_Line.Argument (4);
		Node_Name  	: constant String := Ada.Command_Line.Argument (5);
		Par_3 	  	: constant String := Ada.Command_Line.Argument (6);
		Node_Addr  	: constant String := Ada.Command_Line.Argument (7);

	begin

		Ada.Text_IO.Put_Line("" & Log_Level);

		if(not Logger.Init(Log_Level)) then
			Ada.Text_IO.Put_Line("ERROR : Unknown log level " & Log_Level & "! Use a valid log level [ -i | -n | -d ].");
			Ada.Command_Line.Set_Exit_Status(Ada.Command_Line.Failure);
			return;
		end if;

		if Par_1 /= "-ns" then
			Ada.Text_IO.Put_Line("ERROR : You must specify name server address, invalid option " & Par_1);
			Ada.Command_Line.Set_Exit_Status(Ada.Command_Line.Failure);
			return;
		end if;

		if Par_2 /= "-nn" then
			Ada.Text_IO.Put_Line("ERROR : You must specify node name, invalid option " & Par_2);
			Ada.Command_Line.Set_Exit_Status(Ada.Command_Line.Failure);
			return;
		end if;

		if Par_3 /= "-na" then
			Ada.Text_IO.Put_Line("ERROR : You must specify node address, invalid option " & Par_3);
			Ada.Command_Line.Set_Exit_Status(Ada.Command_Line.Failure);
			return;
		end if;

		declare
			-- Creation of Actors for Travelers
			Params : YAMI.Parameters.Parameters_Collection := YAMI.Parameters.Make_Parameters;

			Client : Message_Agent_Ref := new Message_Agent_Type;

		begin

			Params.Set_String("name",Node_Name);
			Params.Set_String("address",Node_Addr);

	--  		Train_Pool.Associate(1);
	--  		Train_Pool.Associate(2);
	--  		Train_Pool.Associate(3);
	--  		Train_Pool.Associate(4);
	--
			Client.Send(
				Name_Server,
				"name_server",
				"add",
				Params);

			declare
				-- Start the real simulation
				Traveler_Tasks 	: Task_Pool.Task_Pool_Type(5);
				Pool			: Train_Pool.Train_Task_Pool(3);
			begin
				null;
			end;
			Client.Close;

		exception
   			when E : YAMI.Runtime_Error =>
   				Logger.Log(
	   				Sender => "Message_Agent.Message_Agent_Type",
	   				Message => "ERROR : " & Exception_Message(E) & " Impossibile connettersi al Name Server",
	   				L => Logger.ERROR);
	   			Client.Close;
		end;
	end;
exception
	when E : others => Ada.Text_IO.Put_Line(Ada.Exceptions.Exception_Message (E));
end Main;
