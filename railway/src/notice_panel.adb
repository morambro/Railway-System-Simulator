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
--  Railway_Simulation is distributed in the hope that it will be useful,			--
--  but WITHOUT ANY WARRANTY; without even the implied warranty of				--
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the				--
--  GNU General Public License for more details.								--
--																				--
--  You should have received a copy of the GNU General Public License			--
--  along with Railway_Simulation.  If not, see <http://www.gnu.org/licenses/>.		--
----------------------------------------------------------------------------------
with Central_Controller_Interface;
with Logger;
with Gnatcoll.JSON;use Gnatcoll.JSON;

package body Notice_Panel is

	protected body Notice_Panel_Entity is

		procedure Set_Status(
			Event 	: in 		Traveler_Event) is
		begin
			null;
		end Set_Status;

		procedure Set_Status(
			Event 	: in		Train_Event)
		is
			J_Event : JSON_Value := Create_Object;
		begin
			J_Event.Set_Field("type","train_event");
			J_Event.Set_Field("train_id",Event.Train_ID);
			J_Event.Set_Field("station",Event.Station);
			J_Event.Set_Field("platform",Event.Platform);
			J_Event.Set_Field("action","ARRIVE");
			Central_Controller_Interface.Send_Event(J_Event.Write);
		end Set_Status;

		procedure Set_Status(Status : String) is
		begin
			Logger.Log(
				"Station " & Station_ID.all & "-Notice_Panel",
				Status,
				Logger.NOTICE);

		end Set_Status;

	end Notice_Panel_Entity;

end Notice_Panel;
