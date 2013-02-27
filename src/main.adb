--==============================================================================
-- File:
--	main.adb
-- Author:
--	Moreno Ambrosin
--  	Mat.  : 1035635
-- Date:
-- 	09/02/2013
--==============================================================================

with YAMI.Outgoing_Messages; use YAMI.Outgoing_Messages;
with YAMI.Agents;
with YAMI.Parameters;

with Ada.Command_Line;
with Ada.Exceptions;
with Ada.Text_IO;


with Stations;
with Environment;use Environment;
with Trains;
with Route;use Route;
with Train_Pool;


procedure Main is

	-- Reference type must be in the same scope as the type Access
	--
	-- type Any_Refer is access all Operation_Interface'Class;
	--
	-- Prova : Any_Refer := Op_Red1'Access;

	-- Item : Any_Operation;

    --Exec : Executor;
    Pool : Train_Pool.Train_Task_Pool(3);

begin

	Train_Pool.Associate(Trains.Trains(1));
	null;


	--Put_Line("Passenger Name = "& P.GetName);
	--Put_Line("Passenger Surname = "& P.GetSurname);
	--Put_Line("Passenger ID = "& Integer'Image(P.GetID));

	--for I in 1 .. Operations'Length loop

		--Task_Pool.Execute(Operations(I));

	--end loop;


	--T1.Initialize(TD1);
	--T2.Initialize(TD2);
	--T3.Initialize(TD3);
	--T4.Initialize(TD4);

--	Task_Pool.Execute(Traveler1_Operations(Traveler1_Manager.Next_Operation));

--	Task_Pool.Execute(Traveler1_Operations(Traveler1_Manager.Next_Operation));


--	if Ada.Command_Line.Argument_Count /= 1 then
--		Ada.Text_IO.Put_Line("expecting one parameter: server destination");
--		Ada.Command_Line.Set_Exit_Status(Ada.Command_Line.Failure);
--		return;
--	end if;

--	declare
--	  	Server_Address : constant String := Ada.Command_Line.Argument (1);
--	  	Client_Agent : YAMI.Agents.Agent := YAMI.Agents.Make_Agent;
--	begin
--		--  read lines of text from standard input
--		--  and post each one for transmission
--    	while not Ada.Text_IO.End_Of_File loop
--       		declare
--				Input_Line : constant String := Ada.Text_IO.Get_Line;
--				Params : YAMI.Parameters.Parameters_Collection := YAMI.Parameters.Make_Parameters;
--			begin
--		        --  the "content" field name is arbitrary,
--		        --  but needs to be recognized at the server side
--		        Params.Set_String ("content", Input_Line);
--		        Params.Set_String ("prova", "Sciao belo");
--		        Client_Agent.Send_One_Way (Server_Address, "printer", "print", Params);
--		    end;
--      	end loop;
--	end;
exception
	when E : others => Ada.Text_IO.Put_Line(Ada.Exceptions.Exception_Message (E));
end Main;
