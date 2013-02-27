with Ada.Text_IO;use Ada.Text_IO;

package body Track is

	protected body Track_Type is

		entry Leave(T : Train_Descriptor) when true is
		begin
			Put_Line("Train " & Integer'Image(T.ID) & " leaves the Track");
		end Leave;

		entry Enter(To_Add : in out Train_Descriptor; Max_Speed : out Integer) when Free = True is
		begin
			Free := False;
			Max_Speed := Track_Max_Speed;
		end Enter;

	end Track_Type;

end Track;
