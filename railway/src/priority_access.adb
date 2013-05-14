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
with Trains;
with Train;use Train;

package body Priority_Access is

	protected body Priority_Access_Controller is

    	entry Gain_Access(
    		Train_Index : in 	Positive) when True is
    	begin
    		-- # Requeue on the correct Entry
    		if Trains.Trains(Train_Index).T_Type = Train.FB then
    			requeue Gain_Access_FB;
    		else
    			requeue Gain_Access_Regional;
    		end if;
		end Gain_Access;

    	entry Gain_Access_FB(
    		Train_Index : in 	Positive) when Free is
    	begin
    		Free := False;
		end Gain_Access_Fb;

    	entry Gain_Access_Regional(
    		Train_Index : in 	Positive) when Free and Gain_Access_FB'Count = 0 is
    	begin
    		Free := False;
		end Gain_Access_Regional;

	 	procedure Access_Gained is
    	begin
    		Free := True;
		end Access_Gained;

    end Priority_Access_Controller;

end Priority_Access;
