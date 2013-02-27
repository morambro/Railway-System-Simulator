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
	    Id 			    : Integer;
		Speed 		    : Integer;
	    Max_Speed 	    : Integer;
	    -- Id of the current station
	    Current_Station : Positive;
	    -- Id of the next stage;
	    Next_Stage		: Positive;
	End Record;


end Train;
