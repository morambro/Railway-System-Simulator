--==============================================================================
-- File:
--		train.ads
-- Created by:
--		Moreno Ambrosin
--  	Mat.  : 1035635
-- Date:
--		09/02/2013
-- Updates:
--		22/02/2013 : Moved Train Task definition in Train_Pool package
--==============================================================================

with Gnatcoll.JSON;use Gnatcoll.JSON;
with JSON_Helper;use JSON_Helper;

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
	    -- Index of the Route
	    Route_Index		: Positive;
	End Record;

	type Trains_Array is array (Positive range <>) of Train_Descriptor;

	----------------- Procedures to convert Json to Train_Descriptor ----------------------------

	function Get_Trains_Array(Json_File_Name : String) return Trains_Array;

	function Get_Train_Descriptor(Json_Train : Json_Value) return Train_Descriptor;


end Train;
