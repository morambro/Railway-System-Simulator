with Ada.Text_IO;use Ada.Text_IO;

package body Track is

	protected body Track_Type is

		entry Leave(T : Train_Descriptor) when not Free is
		begin
			Free := True;
		end Leave;

		entry Enter(To_Add : in out Train_Descriptor; Max_Speed : out Positive;Leg_Length : out Float) when Free is
		begin
			Free := False;
			Max_Speed := Track_Max_Speed;
			Leg_Length := Track_Length;
		end Enter;

	end Track_Type;

end Track;
