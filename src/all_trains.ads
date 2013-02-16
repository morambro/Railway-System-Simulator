with Generic_Train;use Generic_Train;
with Train;use Train;

package All_Trains is
	TD1 : Train_Descriptor := (ID => 1,Speed => 50,Max_Speed => 100);
	TD2 : Train_Descriptor := (ID => 2,Speed => 50,Max_Speed => 160);
	TD3 : Train_Descriptor := (ID => 3,Speed => 50,Max_Speed => 120);
	TD4 : Train_Descriptor := (ID => 4,Speed => 50,Max_Speed => 80);
	
	T1 : Train_Type;
	T2 : Train_Type;
	T3 : Train_Type;
	T4 : Train_Type;
end All_Trains;
