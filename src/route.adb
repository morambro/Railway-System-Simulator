----------------------------------------------------------------------------------
--  Copyright 2013                                								--
--  Moreno Ambrosin                         									--
--  Railway_Simulation 1.0                                       				--
--  Concurrent and Distributed Systems class project  							--
--  Master Degree in Computer Science                 							--
--  Academic year 2012/2013                              						--
--  Dept. of Pure and Applied Mathematics             							--
--  University of Padua, Italy                        							--
--                                                    							--
--  This file is part of Railway_Simulation project.							--
--																				--		
--  Railway_Simulation is free software: you can redistribute it and/or modify	--
--  it under the terms of the GNU General Public License as published by		--
--  the Free Software Foundation, either version 3 of the License, or			--
--  (at your option) any later version.											--
--																				--
--  Railway_Simulation is distributed in the hope that it will be useful,		--
--  but WITHOUT ANY WARRANTY; without even the implied warranty of				--
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the				--
--  GNU General Public License for more details.								--
--																				--
--  You should have received a copy of the GNU General Public License			--
--  along with Railway_Simulation.  If not, see <http://www.gnu.org/licenses/>. --
----------------------------------------------------------------------------------
package body Route is

	function GetNextTrack (S : Stage) return Positive is
	begin
		return S.Next_Track;
	end GetNextTrack;

	function GetNextStation (S : Stage) return Positive is
	begin
		return S.Next_Station;
	end GetNextStation;

	function GetTimeToLeave (S : Stage) return Time is
	begin
		return S.Leave_At;
    End Gettimetoleave;

    function GetNextPlattform (S : Stage) return Positive is
    begin
		return S.Plattform_Index;
    end GetNextPlattform;

    --
    -- Creates a new Stage with the given parameters and returns it.
    --
    function Newstage(
		      Track : Positive;
		      Station : Positive;
		      Next_Plattform : Positive;
		      Leave_At : Time)
    return Stage is
		To_Return : Stage;
	begin
		To_Return.Next_Track        := Track;
	    To_Return.Next_Station      := Station;
	    To_Return.Plattform_Index   := Next_Plattform;
		To_Return.Leave_At          := Leave_At;
		return To_Return;
	end NewStage;

end Route;
