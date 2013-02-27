with Train;use Train;

--  pragma Elaborate_All(Train_Pool);

package Trains is

	Trains : array (1 .. 2) of Train_Descriptor := (
		1 => (ID => 1,Speed => 50,Max_Speed => 100,Current_Station => 1,Next_Stage => 1),
		2 => (ID => 2,Speed => 50,Max_Speed => 160,Current_Station => 1,Next_Stage => 1)
	);

end Trains;
