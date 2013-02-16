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

	-- Queue used to manage Operations
	-- Operations_Queue : Queue_Type;

	task body Executor is
		--Operation : Operation_Interface'Class := null;
		To_Execute : Any_Operation;
	begin
		loop
			Put_Line("Task waits for an operation to Execute");
			Operations_Queue.Pop(To_Execute);
			
			Put_Line("Task retrieved an Operation");
			
			-- Right Here, I'm shure to have an Operation to Execute
			To_Execute.Do_Operation;
			
			delay 2.0;

			-- After Executing the Operation, Frees the memory
			Free(To_Execute);

		end loop;
	end Executor;

	procedure Execute(Operation : Any_Operation) is
	begin
		Operations_Queue.Push(Operation);
	end Execute;

end Task_Pool;
