with Ada.Numerics.Discrete_Random	;
package Railway.Train is
	
	type Train_Descriptor;
		
	type Train_Descriptor is
	record
		ID 			: Integer;
		Speed 		: Integer;
		Max_Speed 	: Integer;
	end record;	
	
	task type Train_Type is 
		entry Initialize(Descr : Train_Descriptor);
	end Train_Type;

	-- Random initializations
	type Rand_Range is range 1..3;
	package Rand_Int is new Ada.Numerics.Discrete_Random(Rand_Range);
   	seed : Rand_Int.Generator;
   	Num : Rand_Range;
	
end Railway.Train;
