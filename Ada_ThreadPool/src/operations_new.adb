with Ada.Text_IO;
with Ada.Strings.Unbounded;

package body Operations_New is

	use Ada.Text_IO;

	procedure Do_Operation(O: in Operation_Type_New) is
	begin
		Put_Line("The name is (Operation_New): " & Ada.Strings.Unbounded.To_String(O.Name));
	end;

	function Make(Name : String) return Operation_Type_New is
		Op : Operation_Type_New;
	begin
		Op.SetName(Name);
		return Op;
	end;

	procedure SetName(O: in out Operation_Type_New;S:String ) is
	begin
		O.Name := US.To_Unbounded_String(S);
	end;


end Operations_New;
