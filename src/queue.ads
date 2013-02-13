--==============================================================================
-- File:
--	queues.ads
-- Created by:
--	Moreno Ambrosin
--  	Mat.  : 1035635
-- Date:
-- 	09/02/2013
--==============================================================================

--
-- Declaration of a generic queue implementation of elements of type Element
--
generic type Element is private;
package Queue is

	-- Queue Item's type
	type Queue_Item is private;
	-- Reference to an Item of the queue, to implement it as a List of arbitrary Length
	type Queue_Item_Ref is access all Queue_Item;

	--
	-- Queue Type represented as a Protected Resource.
	--
	protected type Queue_Type is
		--
		-- Removes the first element from the Queue, and puts
		-- it in Item parameter
		--
		entry Pop(Item : out Element);
		--
		-- Adds the given Item to the queue
		--
		procedure Push(Item : Element);
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
		Item : Element;
		Next : Queue_Item_Ref ;
	end record;

end Queue;
