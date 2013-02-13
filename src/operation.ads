--==============================================================================
-- File:
--	operations.ads
-- Created by:
--	Moreno Ambrosin
--  	Mat.  : 1035635
-- Date:
-- 	09/02/2013
--==============================================================================

with Ada.Strings.Unbounded;
with Generic_Operation_Interface;use Generic_Operation_Interface;

-- This package contains an implementation of generic Operation Interface
package Operation is

	package US renames Ada.Strings.Unbounded;

	--
	-- Class type implementing Operation Interface
	--
	type Operation_Type is new Operation_Interface with private;

	--
	-- Declare the override of Operation to be done
	--
	overriding procedure Do_Operation(O: in  Operation_Type);

	--
	-- Additional methods
	--
	procedure SetName(O: in out Operation_Type; S: String);

	--
	-- Method used as a contructor
	--
	function Operation(Name : String) return Operation_Type;

	function NewOperation(Name : String) return Any_Operation;


private
	-- Private fields for Operation_Type
	type Operation_Type is new Operation_Interface with
	record
		Name : US.Unbounded_String;
	end record;

end Operation;
