with Queue;
with Train;use Train;
with Ada.Numerics.Discrete_Random;

package Train_Pool is

	task type Train_Type is 
		entry Initialize(Descr : Train_Descriptor);
	end Train_Type;
	
	type Train_Vector is array (Positive range <>) of Train_Type;

	type Train_Task_Pool(Pool_Size : Positive) is limited private; 
	
	procedure Associate(Train : Train_Descriptor);
	
private 
	
	-- Declaration of a new Queue package with Element => Train_Descriptor
	package Trains_Queue_Package is new Queue(Element => Train_Descriptor);
	
	-- Queue used to manage Traveler operations
	Trains_Queue : Trains_Queue_Package.Unbounded_Queue.Queue;
	
		-- Random initializations
	type Rand_Range is range 1..3;
	package Rand_Int is new Ada.Numerics.Discrete_Random(Rand_Range);
   	seed : Rand_Int.Generator;
   	Num : Rand_Range;
	
	type Train_Task_Pool(Pool_Size : Positive) is record
		Tasks : Train_Vector(1 .. Pool_Size);
	end record;
	
end Train_Pool;
