--==============================================================================
-- File:
--	task_pool.adb
-- Created by:
--	Moreno Ambrosin
--  	Mat.  : 1035635
-- Date:
-- 	09/02/2013
--==============================================================================
with Logger;

package body Task_Pool is

	--
	-- Task type Executor definition
	--
	task body Actor is

		NAME : constant String := "Task_Pool.Actor";

		To_Execute : Any_Operation;

	begin
		loop
			Logger.Log(NAME,"Task waits for an operation to Execute",Logger.VERY_VERBOSE);
			Operations_Queue.Dequeue(To_Execute);

			Logger.Log(NAME,"Task retrieved an Operation",Logger.VERY_VERBOSE);

			-- Right Here, I'm shure to have an Operation to Execute
			To_Execute.Do_Operation;

		end loop;
    end Actor;

	--
	-- Adds the given Operation Pointer to the Operations queue
	--
	procedure Execute(Operation : Any_Operation) is
	begin
		Operations_Queue.Enqueue(Operation);
	end Execute;

end Task_Pool;
