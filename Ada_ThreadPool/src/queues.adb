--==============================================================================
-- File:
--	queues.adb
-- Created by:
--	Moreno Ambrosin
--  	Mat.  : 1035635
-- Date:
-- 	09/02/2013
--==============================================================================

with Operation_Interfaces;use Operation_Interfaces;
with Ada.Text_IO; use Ada.Text_IO;
package body Queues is

	protected body Queue_Type is

		entry Pop(Item : out Any_Operation) when Head /= Null is
			ToReturn : Queue_Item_Ref := Head;
		begin

			Head := Head.Next;
			-- Item to return
			Item := ToReturn.Item;

			Queue_Size := Queue_Size - 1;

			if(Queue_Size = 0)then
				Tail := Head;
			end if;

		end Pop;

		procedure Push(Item : Any_Operation) is
			New_Elem : Queue_Item_Ref := new Queue_Item;
		begin

			New_Elem.Next := Null;
			New_Elem.Item := Item;

			if(Queue_Size = 0) then
				Tail := New_Elem;
				Head := Tail;
			else
				Tail.Next := New_Elem;
				Tail := New_Elem;
			end if;

			Queue_Size := Queue_Size + 1;


		end Push;

		function GetSize return Integer is
		begin
			return Queue_Size;
		end GetSize;

	end Queue_Type;

end Queues;
