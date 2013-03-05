with Ada.Text_IO;use Ada.Text_IO;

package body Track is

	protected body Track_Type is

		entry Enter(To_Add : in out Train_Descriptor; Max_Speed : out Positive;Leg_Length : out Float) when True is
		begin

			Put_Line ("Current direction : " & Integer'Image(To_Add.Current_Station) );

			if Free then
				Put_Line("Train " & Integer'Image(To_Add.ID) & " Enters, Track was free");
				Current_Direction := To_Add.Current_Station;
				Free := False;
			else
				if ( To_Add.Current_Station /= Current_Direction) then
					-- In case The track is not null, move the task to a waiting queue
					Put_Line("Train " & Integer'Image(To_Add.ID) & " will be re-queued, wrong direction");
					requeue Wait;
				end if;
			end if;

			-- Add the train to the train queue
			Trains_Number := Trains_Number + 1;
			Running_Trains(Trains_Number) := To_Add.ID;

			-- Set parameters
			Max_Speed := Track_Max_Speed;
			Leg_Length := Track_Length;
			Put_Line("Train " & Integer'Image(To_Add.ID) & " added to running trains queue");

		end Enter;

		entry Wait(To_Add : in out Train_Descriptor; Max_Speed : out Positive;Leg_Length : out Float)
			when Can_Retry_Enter is
		begin

			if Free then
				Put_Line("Train " & Integer'Image(To_Add.ID) & " Enters, Track was free");
				Free := False;
				Current_Direction := To_Add.Current_Station;
			end if;
			-- Add the train to the train queue
			Trains_Number := Trains_Number + 1;
			Running_Trains(Trains_Number) := To_Add.ID;

			-- Set parameters
			Free := False;
			Max_Speed := Track_Max_Speed;
			Leg_Length := Track_Length;

			Put_Line("Train " & Integer'Image(To_Add.ID) & " added to running trains queue");

		end Wait;

-- ################################################################################################

		entry Leave(T : Train_Descriptor) when not Free is
		begin
			if(Running_Trains(1) = T.ID) then
				for I in Integer range 2 .. 10 loop
					Running_Trains(I-1) := Running_Trains(I);
				end loop;
				if(Retry'Count > 0) then
					Retry_Num := Retry'Count;
					Can_Retry_Leave := True;
				end if;
				Put_Line("Train " & Integer'Image(T.ID) & " Leaves!");
				Trains_Number := Trains_Number - 1;

				if( Trains_Number = 0 ) then
					if( Wait'Count > 0) then
						Can_Retry_Enter := True;
					else
						Free := True;
						Current_Direction := 0;
					end if;
				end if;

			else
				Put_Line("Train " & Integer'Image(T.ID) & " Cannot leave because it's not the first");
				requeue Retry;
			end if;
		end Leave;

		entry Retry(T : Train_Descriptor) when Can_Retry_Leave is
		begin

			Retry_Num := Retry_Num - 1;
			if(Retry_Num = 0) then
				Can_Retry_Leave := False;
			end if;

			if(Running_Trains(1) = T.ID) then
				for I in Integer range 2 .. 10 loop
					Running_Trains(I-1) := Running_Trains(I);
				end loop;
				if(Retry'Count > 0) then
					Retry_Num := Retry'Count;
					Can_Retry_Leave := True;
				end if;
				Put_Line("Train " & Integer'Image(T.ID) & " Leaves!");
				Trains_Number := Trains_Number - 1;

				if( Trains_Number = 0 ) then
					if( Wait'Count > 0) then
						Can_Retry_Enter := True;
					else
						Free := True;
						Current_Direction := 0;
					end if;
				end if;

			else
				Put_Line("Train " & Integer'Image(T.ID) & " Cannot leave because it's not the first");
				requeue Retry;
			end if;
		end Retry;


	end Track_Type;


end Track;
