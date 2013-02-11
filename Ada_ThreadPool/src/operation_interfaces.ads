--==============================================================================
-- File:
--	operation_interface.ads
-- Created by:
--	Moreno Ambrosin
--  	Mat.  : 1035635
-- Date:
-- 	09/02/2013
--==============================================================================

with Unchecked_Deallocation;

-- Package which defines an Interface to expose a unique method, to be invoked
-- by worker tasks.
package Operation_Interfaces is

	-- Operation Interface type declaration
	type Operation_Interface is abstract tagged null record;

	-- Operation Method declaration
	procedure Do_Operation (X : in Operation_Interface) is abstract;

   	-- Operation reference type to be used inside records: Type'Class doesn't
   	-- have a fixed size so it can be not allocated inside a record.
   	type Any_Operation is access all Operation_Interface'Class;

	--
 	procedure Free is new Unchecked_Deallocation (
      		Operation_Interface'Class,
		Any_Operation
	);

   	pragma Controlled (Any_Operation);

end Operation_Interfaces;
