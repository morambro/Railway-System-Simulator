with Ada.Text_IO;use Ada.Text_IO;

package body Notice_Panel is
	
	protected body Notice_Panel_Entity is 
		
		procedure SetStatus(Status : String) is 
		begin
			Put_Line("Panel says : " & Status);
		end SetStatus;
		
	end Notice_Panel_Entity;

end Notice_Panel;
