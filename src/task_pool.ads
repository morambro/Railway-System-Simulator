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

	-- Adds a new operation to the executing queue	
	procedure Execute(Operation : Any_Operation);

	-- Task which Executes an operation taken from Operations_Queue
	task type Executor;

	type Executors_Vector is array(Integer range <>) of Executor;
	
	type Task_Pool_Type(Pool_Dimension : Positive) is record
		Executors : Executors_Vector(1..Pool_Dimension);
	end record;

private 
	
	-- Declaration of a new Queue package with Element => Any_Operation
	package Operations_Queue_Package is new Queue(Element => Any_Operation);
	
	-- Queue used to manage Traveler operations
	Operations_Queue : Operations_Queue_Package.Unbounded_Queue.Queue;

	
end Task_Pool;
