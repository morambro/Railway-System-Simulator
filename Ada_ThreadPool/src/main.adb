-------------------------------------------------------------------------------
-- File:
--	main.adb
-- Author:
--	Moreno Ambrosin
--  	Mat.  : 1035635
-- Date:
-- 	09/02/2013
-------------------------------------------------------------------------------

with Operations;use Operations;
with Operations_New;use Operations_New;
with Operation_Interfaces;use Operation_Interfaces;
with Task_Pool;
with Queues;use Queues;

procedure Main is

	-- Cobegin with Tasks In the Task Pool
	use Task_Pool;

	Op_Red1 : aliased Operation_Interface'Class := Operation(Name => "Lake");
	Op_Red2 : Operation_Interface'Class := Operation(Name => "Gero");
	Op_Red3 : Operation_Interface'Class := Operation(Name => "Wez");

	Boh : Any_Operation := null;

	-- Reference type must be in the same scope as the type Access
	--
	-- type Any_Refer is access all Operation_Interface'Class;
	--
	-- Prova : Any_Refer := Op_Red1'Access;

	Queue : Queue_Type;

	-- Item : Any_Operation;

	--Exec : Executor;

begin

	Boh := new Operation_Type;

	delay 3.0;

	Task_Pool.Init;

	Task_Pool.Execute(Boh);

	Task_Pool.Execute(new Operation_Type_New);

end Main;
