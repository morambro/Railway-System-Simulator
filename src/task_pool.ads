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

	task type Executor(ID : Integer);

	procedure Execute(Operation : Any_Operation);

	procedure Init;

private
	-- Static thread Pool
	Executor1 : Executor(1);
	Executor2 : Executor(2);
	Executor3 : Executor(3);
	Executor4 : Executor(4);

	-- type Executors_Vector is array(Integer range <>) of Executor();

end Task_Pool;
