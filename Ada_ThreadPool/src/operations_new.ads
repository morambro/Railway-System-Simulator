with Ada.Strings.Unbounded;
with Operation_Interfaces;use Operation_Interfaces;

package Operations_New is

	package US renames Ada.Strings.Unbounded;

	type Operation_Type_New is new Operation_Interface with private;--tagged private;

	overriding procedure Do_Operation(O: in Operation_Type_New);

	function Make(Name : String) return Operation_Type_New;

	procedure SetName(O: in out Operation_Type_New; S: String);

	private type Operation_Type_New is new Operation_Interface with-- tagged
	record
		Name : US.Unbounded_String;
	end record;

end Operations_New;
