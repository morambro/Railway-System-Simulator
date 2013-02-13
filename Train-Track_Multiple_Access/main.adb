with Railway.Track; use Railway.Track;
with Railway.Train; use Railway.Train;


procedure Main is
	Track_1 : Track;
	TD1 : Train_Descriptor := (ID => 1,Speed => 50,Max_Speed => 100);
	TD2 : Train_Descriptor := (ID => 2,Speed => 50,Max_Speed => 160);
	TD3 : Train_Descriptor := (ID => 3,Speed => 50,Max_Speed => 120);
	TD4 : Train_Descriptor := (ID => 4,Speed => 50,Max_Speed => 80);
	
	T1 : Train_Type;
	T2 : Train_Type;
	T3 : Train_Type;
	T4 : Train_Type;
	
begin
	
	T1.Initialize(TD1);
	T2.Initialize(TD2);
	T3.Initialize(TD3);
	T4.Initialize(TD4);
	
end Main;
