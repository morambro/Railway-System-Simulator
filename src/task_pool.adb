--==============================================================================
-- File:
--	task_pool.adb
-- Created by:
--	Moreno Ambrosin
--  	Mat.  : 1035635
-- Date:
-- 	09/02/2013
--==============================================================================
with Ada.Text_IO;use Ada.Text_IO;

package body Task_Pool is

	--
	-- Task type Executor definition
	--
	task body Executor is
		--Operation : Operation_Interface'Class := null;
		To_Execute : Any_Operation;
	begin
		loop
			Put_Line("Task waits for an operation to Execute");
			Operations_Queue.Dequeue(To_Execute);
			
			Put_Line("Task retrieved an Operation");
			
			-- Right Here, I'm shure to have an Operation to Execute
			To_Execute.Do_Operation;

		end loop;
	end Executor;

	--
	-- Adds the given Operation Pointer to the Operations queue
	--
	procedure Execute(Operation : Any_Operation) is
	begin
		Operations_Queue.Enqueue(Operation);
	end Execute;

end Task_Pool;
