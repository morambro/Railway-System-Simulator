--==============================================================================
-- File:
--	operations.adb
-- Created by:
--		Moreno Ambrosin
--  	Mat.  : 1035635
-- Date:
-- 	09/02/2013
--==============================================================================

with Ada.Text_IO;
with Ada.Strings.Unbounded;

package body Operations is

	use Ada.Text_IO;
	--
	-- Do_Operation Override body
	--
	procedure Do_Operation(O: in Operation_Type) is
	begin
		Put_Line("The name is (Operation): " & Ada.Strings.Unbounded.To_String(O.Name));
	end;

	--
	-- Function used to create a new instance of Operation_Type
	--
	function Operation(Name : String) return Operation_Type is
		Op : Operation_Type;
	begin
		Op.SetName(Name);
		return Op;
	end;

	procedure SetName(O: in out Operation_Type;S:String ) is
	begin
		O.Name := US.To_Unbounded_String(S);
	end;


end Operations;
