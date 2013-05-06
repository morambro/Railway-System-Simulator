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
with Message_Agent;
with YAMI.Parameters;

package Central_Controller_Interface is

	type Traveler_Action is (ENTER,LEAVE,FINISHED);

	type Train_Action is (ENTER,LEAVE);

	procedure Send_Event(Json_Event : String);

	procedure Set_Train_Status(
			Train		: Positive;
			Station		: String;
			Platform	: Positive;
			Time		: Positive;
			Segment		: Positive;
			Action		: Train_Action);

	procedure Set_Traveler_Status(
		Traveler	: Positive;
		Train		: Positive;
		Station		: String;
		Platform	: Positive;
		Action 		: Traveler_Action);

	-- #
	-- # This procedure is used to notify the termination of the current Node
	-- # to the Controller, to complete the termination procedure.
	-- #
	procedure Notify_Termination;

end Central_Controller_Interface;