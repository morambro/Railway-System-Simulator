with Train;use Train;

package Trains is

	Trains : array (1 .. 4) of Train_Descriptor := (
		1 => (ID => 1111,Speed => 50,Max_Speed => 100,Current_Station => 1,Next_Stage => 1),
		2 => (ID => 2222,Speed => 50,Max_Speed => 160,Current_Station => 1,Next_Stage => 1),
		3 => (ID => 3333,Speed => 50,Max_Speed => 160,Current_Station => 1,Next_Stage => 1),
		4 => (ID => 4444,Speed => 50,Max_Speed => 160,Current_Station => 1,Next_Stage => 1)
	);

end Trains;
