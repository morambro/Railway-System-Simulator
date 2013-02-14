--==============================================================================
-- File:
--	main.adb
-- Author:
--	Moreno Ambrosin
--  	Mat.  : 1035635
-- Date:
-- 	09/02/2013
--==============================================================================

with Generic_Operation_Interface;use Generic_Operation_Interface;
with Task_Pool;
with Railway.Track; use Railway.Track;
with Railway.Train; use Railway.Train;
with Ada.Text_IO;use Ada.Text_IO;
with Passenger;use Passenger;

procedure Main is

	-- Cobegin with Tasks In the Task Pool
	use Task_Pool;


	Boh : Any_Operation := null;

	-- Reference type must be in the same scope as the type Access
	--
	-- type Any_Refer is access all Operation_Interface'Class;
	--
	-- Prova : Any_Refer := Op_Red1'Access;

	-- Item : Any_Operation;

	--Exec : Executor;

	-- Train-Track 
	Track_1 : Track;
	TD1 : Train_Descriptor := (ID => 1,Speed => 50,Max_Speed => 100);
	TD2 : Train_Descriptor := (ID => 2,Speed => 50,Max_Speed => 160);
	TD3 : Train_Descriptor := (ID => 3,Speed => 50,Max_Speed => 120);
	TD4 : Train_Descriptor := (ID => 4,Speed => 50,Max_Speed => 80);
	
	T1 : Train_Type;
	T2 : Train_Type;
	T3 : Train_Type;
	T4 : Train_Type;
	
	--P : aliased Passenger_Type := NewPassengerType(5,1,"Moreno","Ambrosin");

	--Operations : Passenger_Operations := P.GetOperations;
	
	P2 : Passenger_Ref := NewPassenger(5,2,"Nicola","Geromel");
	
begin


	Task_Pool.Init;
	
	--Put_Line("Passenger Name = "& P.GetName);
	--Put_Line("Passenger Surname = "& P.GetSurname);
	--Put_Line("Passenger ID = "& Integer'Image(P.GetID));
	
	--for I in 1 .. Operations'Length loop
		
		--Task_Pool.Execute(Operations(I));
		
	--end loop;
	
	for I in 1 .. P2.GetOperations'Length loop
		
		Task_Pool.Execute(P2.GetOperations(I));
		
	end loop;
	

	--T1.Initialize(TD1);
	--T2.Initialize(TD2);
	--T3.Initialize(TD3);
	--T4.Initialize(TD4);

end Main;
