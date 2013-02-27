with Train;use Train;

with Train_Pool;use Train_Pool;

pragma Elaborate_All(Train_Pool);

package Trains is

	Trains : array (1 .. 4) of Train_Descriptor := (
		1 => (ID => 1,Speed => 50,Max_Speed => 100),
		2 => (ID => 2,Speed => 50,Max_Speed => 160),
		3 => (ID => 3,Speed => 50,Max_Speed => 120),
		4 => (ID => 4,Speed => 50,Max_Speed => 80)
	);

	Pool : Train_Task_Pool(3);

end Trains;
