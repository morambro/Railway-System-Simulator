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
with Route; use Route;
with Ada.Real_Time;

package Routes is

    Route : Route_Type(1..2) := (
		1 => Newstage(Track => 1,Station => 2,Next_Plattform => 1,Leave_At => Ada.Real_Time.Clock),
		2 => Newstage(Track => 2,Station => 3,Next_Plattform => 1,Leave_At => Ada.Real_Time.Clock)
--  		3 => Newstage(Track => 3,Station => 4,Next_Plattform => 1,Leave_At => Ada.Real_Time.Clock)
	);


	Rev_Route : Route_Type(1..2) := (
--  		1 => Newstage(Track => 3,Station => 4,Next_Plattform => 1,Leave_At => Ada.Real_Time.Clock),
		2 => Newstage(Track => 2,Station => 3,Next_Plattform => 1,Leave_At => Ada.Real_Time.Clock),
		3 => Newstage(Track => 1,Station => 2,Next_Plattform => 1,Leave_At => Ada.Real_Time.Clock)
	);

end Routes;