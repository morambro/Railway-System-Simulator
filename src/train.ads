--==============================================================================
-- File:
--		train.ads
-- Created by:
--		Moreno Ambrosin
--  	Mat.  : 1035635
-- Date:
--		09/02/2013
-- Updates:
--		22/02/2013 : Moved Train Task defintion in Train_Pool package		
--==============================================================================

package Train is
	
	type Train_Descriptor is
	record
		ID 			: Integer;
		Speed 		: Integer;
		Max_Speed 	: Integer;
	end record;	

	
end Train;
