--==============================================================================
-- File:
--		train_pool.ads
-- Created by:
--		Moreno Ambrosin
--  	Mat.  : 1035635
-- Date:
--		22/02/2013
-- Updates:
--
--==============================================================================

with Queue;
with Train;use Train;
with Ada.Numerics.Discrete_Random;

-- #
-- # This Package contains a definition of a Train task pool; Each Task is responsible
-- # of doing certain operations defined in the task body, for a certain Train_Descriptor
-- #
package Train_Pool is

	task type Train_Type;

	type Train_Vector is array (Positive range <>) of Train_Type;

	type Train_Task_Pool(Pool_Size : Positive) is limited private;

	-- #
	-- # Procedure used to add a new descriptor to the queue
	-- #
	procedure Associate(Train : Train_Descriptor);

private

	-- #
	-- # Declaration of a new Queue package with Element => Train_Descriptor
	-- #
	package Trains_Queue_Package is new Queue(Element => Train_Descriptor);

	-- #
	-- # Queue used to manage Traveler operations
	-- #
	Trains_Queue : Trains_Queue_Package.Unbounded_Queue.Queue;

	-- Random initializations
	type Rand_Range is range 1..3;
	package Rand_Int is new Ada.Numerics.Discrete_Random(Rand_Range);
   	seed : Rand_Int.Generator;
   	Num : Rand_Range;

	-- #
	-- # Private Definition of Train_Task_Pool type as a record with one
	-- # field of type Train_Vector
	-- #
	type Train_Task_Pool(Pool_Size : Positive) is record
		Tasks : Train_Vector(1 .. Pool_Size);
	end record;

end Train_Pool;
