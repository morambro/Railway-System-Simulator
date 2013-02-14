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
		Head : access Queue_Item := Null;
		Tail : access Queue_Item := Null;
		Queue_Size : Integer := 0;
	end Queue_Type;


private
	type Queue_Item is
	record
		Item : Element;
		Next : access Queue_Item ;
	end record;

end Queue;
