--==============================================================================
-- File:
--	route.ads
-- Created by:
--	Moreno Ambrosin
--  	Mat.  : 1035635
-- Date:
-- 	22/02/2013
--==============================================================================
--
-- This package contains a representation of a Route for a Train as an unbounded array
-- of Stage objects.
--
with Ada.Real_Time;use Ada.Real_Time;
package Route is

	type Stage is private;

	type Route_Type is array (Positive range <>) of Stage;

	function GetNextTrack (S : Stage) return Positive;
	function GetNextStation (S : Stage) return Positive;
	function GetTimeToLeave (S : Stage) return Time;
    function GetNextPlattform (S : Stage) return Positive;

    function Newstage(
		Track : Positive;
		Station : Positive;
		Next_Plattform : Positive;
		Leave_At : Time) return Stage;

private

	type Stage is record
	    -- Indexes of next Track and Station
		Next_Track      : Positive;
	    Next_Station    : Positive;
        Plattform_Index : Positive;
		Leave_At        : Time;
	end record;


end Route;
