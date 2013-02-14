--==============================================================================
-- File:
--	task_pool.ads
-- Created by:
--	Moreno Ambrosin
--  	Mat.  : 1035635
-- Date:
-- 	09/02/2013
--==============================================================================

with Generic_Operation_Interface;use Generic_Operation_Interface;
with Queue;

package Task_Pool is

	package Operations_Queue_Package is new Queue(Any_Operation);

	Operations_Queue : Operations_Queue_Package.Queue_Type;

	task type Executor;--(ID : Integer);

	procedure Execute(Operation : Any_Operation);

	type Executors_Vector is array(Integer range <>) of Executor;
	
	type Task_Pool_Type(Pool_Dimension : Positive) is record
		Executors : Executors_Vector(1..Pool_Dimension);
	end record;
	
end Task_Pool;
