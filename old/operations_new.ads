--==============================================================================
-- File:
--	operations_new.ads
-- Created by:
--	Moreno Ambrosin
--  	Mat.  : 1035635
-- Date:
-- 	10/02/2013
--==============================================================================

with Ada.Strings.Unbounded;
with Generic_Operation_Interface;use Generic_Operation_Interface;

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
