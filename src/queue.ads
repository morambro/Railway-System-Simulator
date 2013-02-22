--==============================================================================
-- File:
--	queues.ads
-- Created by:
--	Moreno Ambrosin
--  	Mat.  : 1035635
-- Date:
-- 	09/02/2013
--==============================================================================

with Ada.Containers.Unbounded_Synchronized_Queues;
with Ada.Containers.Synchronized_Queue_Interfaces;
--
-- Declaration of a generic queue implementation of elements of type Element
--
generic type Element is private;

package Queue is

	package Unbounded_Queue_Interface is new Ada.Containers.Synchronized_Queue_Interfaces(Element_Type => Element);
	
	package Unbounded_Queue is new Ada.Containers.Unbounded_Synchronized_Queues 
		(Queue_Interfaces => Unbounded_Queue_Interface); 

end Queue;
