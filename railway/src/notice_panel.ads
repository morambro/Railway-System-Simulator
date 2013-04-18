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
with Ada.Strings.Unbounded;use Ada.Strings.Unbounded;

package Notice_Panel is

	type Traveler_Action is (GO,ARRIVE);

	type Traveler_Event is record
		Name 		: Unbounded_String;
		Surname 	: Unbounded_String;
		Station		: Unbounded_String;
		Platform 	: Positive;
		Action		: Traveler_Action;
    end record;

	type Train_Action is (ARRIVE,LEAVE,STOP,PASS);

	type Train_Event is record
		Train_ID	: Integer;
		Station		: Unbounded_String;
		Platform 	: Positive;
		Action		: Train_Action;
    end record;



	protected type Notice_Panel_Entity(Station_ID : access String) is

		procedure Set_Status(
			Event 	: in 		Traveler_Event);

		procedure Set_Status(
			Event 	: in		Train_Event);

		procedure Set_Status(
			Status 	: in		String);

	end Notice_Panel_Entity;

end Notice_Panel;
