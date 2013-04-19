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
with Environment;
with Gnatcoll.JSON;use Gnatcoll.JSON;
with Ada.Text_IO;

package body Central_Controller_Interface is

	procedure Send_Event(Json_Event : String) is

		Parameters : YAMI.Parameters.Parameters_Collection := YAMI.Parameters.Make_Parameters;

	begin

		Parameters.Set_String("event",Json_Event);

		Message_Agent.Instance.Send_One_Way(
			Destination_Address => Environment.Get_Central_Controller,
			Object 				=> "central_controller",
			Service 			=> "event",
			Params 				=> Parameters);

	exception
		-- # Ignore errors
		when E : others => null;

    end Send_Event;


    procedure Set_Train_Status(
		Train		: Positive;
		Station		: String;
		Platform	: Positive;
		Time		: Positive;
		Segment		: Positive;
		Action		: Train_Action)
	is
		J_Event : JSON_Value := Create_Object;
	begin
		J_Event.Set_Field("type","train_event");
		J_Event.Set_Field("time",Time);
		J_Event.Set_Field("segment",Segment);
		J_Event.Set_Field("train_id",Train);
		J_Event.Set_Field("station",Station);
		J_Event.Set_Field("platform",Platform);
		J_Event.Set_Field("action",(if (Action = ENTER) then "enter" else "leave"));

		--Ada.Text_IO.Put_Line(J_Event.Write);

		Central_Controller_Interface.Send_Event(J_Event.Write);
	end Set_Train_Status;

	procedure Set_Traveler_Status(
		Traveler	: Positive;
		Train		: Positive;
		Station		: String;
		Platform	: Positive;
		Action 		: Traveler_Action)
	is
		J_Event : JSON_Value := Create_Object;
	begin
		J_Event.Set_Field("type","traveler_event");
		J_Event.Set_Field("traveler_id",Traveler);
		J_Event.Set_Field("train_id",Train);
		J_Event.Set_Field("station",Station);
		J_Event.Set_Field("platform",Platform);
		J_Event.Set_Field("action",(if (Action = ENTER) then "enter" else "leave"));
		Central_Controller_Interface.Send_Event(J_Event.Write);
	end Set_Traveler_Status;


end Central_Controller_Interface;