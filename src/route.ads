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
-- This package contains a representation of a Route as an unbounded array
-- of Stage objects.
--
package Route is

	type Stage is private;
	
	type Route_Type is array (Positive range <>) of Stage;

--	function GetNextTrack (S : Stage) return Positive;
--	function GetNextStation (S : Stage) return Positive;
--	-- TODO: Change to Time
--	function GetTimeToLeave (S : Stage) return Integer;
	
	function NewStage(T : Positive;S : Positive;Leave_At : Integer) return Stage;

private

	type Stage is record
	-- Indexes of next Track and Station
		Next_Track : Positive;
		Next_Station : Positive;
		-- TODO: Represent in Time
		Leave_At : Integer;
	end record;
	
	
end Route;
