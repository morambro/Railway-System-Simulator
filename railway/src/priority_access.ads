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

-- #
-- # This package contains the definition of the protected type
-- # Priority_Access, which is used to give FB Trains a preference
-- # in resource access.
-- #
package Priority_Access is

	-- #
	-- # This protected type is used to perform Train's access priority.
	-- # The main idea is to let tasks enqueue to the proper entry (Gain_Access_FB
	-- # or Gain_Access_Regional) and to subordinate tasks waiting on Gain_Access_Regional
	-- # execution to Gain_Access_FB queue status.
	-- #
	protected type Priority_Access_Controller is

		-- #
		-- # Main entry used to gain access.
		-- #
    	entry Gain_Access(
    		Train_Index : in 	Positive);

		-- #
		-- # Procedure used to free the resource.
		-- #
    	procedure Access_Gained;

    private

		-- #
		-- # Entry used to enqueue FB Trains
		-- #
    	entry Gain_Access_FB(
    		Train_Index : in 	Positive);

		-- #
		-- # Entry used to enqueue Regional Trains.
		-- # A task will access this entry only if the
		-- # resource is Free and no tasks will be waiting on
		-- # Gain_Access_FB entry.
		-- #
    	entry Gain_Access_Regional(
    		Train_Index : in 	Positive);

		-- # Status of the resource.
		Free : Boolean := True;

    end Priority_Access_Controller;


end Priority_Access;
