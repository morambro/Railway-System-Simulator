--==============================================================================
-- File:
--	task_pool.ads
-- Created by:
--	Moreno Ambrosin
--  	Mat.  : 1035635
-- Date:
-- 	09/02/2013
--==============================================================================

with Operation_Interfaces;use Operation_Interfaces;

package Task_Pool is

	task type Executor(ID : Integer);

	procedure Execute(Operation : Any_Operation);

	procedure Init;

private
	Executor1 : Executor(1);
	Executor2 : Executor(2);
	Executor3 : Executor(3);
	Executor4 : Executor(4);
	-- type Executors_Vector is array(Integer range <>) of Executor();
end Task_Pool;
