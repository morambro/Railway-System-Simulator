with Railway.Track; use Railway.Track;
with Railway.Train; use Railway.Train;


procedure Main is
	Track_1 : Track;
	TD1 : Train_Descriptor := (ID => 6);
	TD2 : Train_Descriptor := (ID => 3);
	TD3 : Train_Descriptor := (ID => 65);
	TD4 : Train_Descriptor := (ID => 67);
begin
	Track_1.Enter(TD1);
	Track_1.Enter(TD2);
	Track_1.Enter(TD3);
	Track_1.Enter(TD4);
	
	Track_1.Leave(TD2);
	Track_1.Leave(TD1);
	Track_1.Leave(TD4);
	Track_1.Leave(TD3);
	
end Main;
