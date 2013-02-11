--==============================================================================
-- File:
--	queues.ads
-- Created by:
--	Moreno Ambrosin
--  	Mat.  : 1035635
-- Date:
-- 	09/02/2013
--==============================================================================

with Operation_Interfaces;use Operation_Interfaces;

package Queues is

	type Queue_Item is private;
	type Queue_Item_Ref is access all Queue_Item;

	--
	-- Queue Type represented as a Protected Resource.
	--
	protected type Queue_Type is
		--
		-- Removes the first element from the Queue, and puts
		-- it in Item parameter
		--
		entry Pop(Item : out Any_Operation);
		--
		-- Adds the given Item to the queue
		--
		procedure Push(Item : Any_Operation);
		--
		-- Returns the current size of the queue
		--
		function GetSize return Integer;
	private
		Head : Queue_Item_Ref := Null;
		Tail : Queue_Item_Ref := Null;
		Queue_Size : Integer := 0;
	end Queue_Type;


private
	type Queue_Item is
	record
		Item : Any_Operation;
		Next : Queue_Item_Ref ;
	end record;

end Queues;
