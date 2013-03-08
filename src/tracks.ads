--==============================================================================
-- Created by:
--		Moreno Ambrosin
--  	Mat.  : 1035635
-- Date:
-- 		08/03/2013
--==============================================================================
with Track;

package Tracks is

	Tracks : access Track.Tracks_Array := Track.Get_Track_Array("res/tracks.json");

end Tracks;