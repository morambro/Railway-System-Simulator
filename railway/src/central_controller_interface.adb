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


	procedure Set_Train_Arriving_Status(
		Station		: in	String;
		Train_ID	: in 	Integer;
		Platform	: in 	Integer;
		Action		: in 	Train_Action;
		Time		: in 	String;
		Train_Delay	: in 	Integer)
	is
		J_Event : JSON_Value := Create_Object;
	begin
		J_Event.Set_Field("type","train_arriving");
		J_Event.Set_Field("train_id",Train_ID);
		J_Event.Set_Field("station",Station);
		J_Event.Set_Field("platform",Platform);
		J_Event.Set_Field("action",(if (Action = ENTER) then "enter" else "leave"));
		J_Event.Set_Field("time",Time);
		J_Event.Set_Field("delay",Train_Delay);

		Send_Event(J_Event.Write);

    end Set_Train_Arriving_Status;


	procedure Set_Train_Arrived_Status(
		Train_ID 	: in 	Integer;
		Station		: in 	String;
		Segment 	: in 	Integer;
		Platform 	: in 	Integer;
		Time		: in 	String;
		Train_Delay	: in 	Integer)
	is
		J_Event : JSON_Value := Create_Object;
	begin
		J_Event.Set_Field("type","train_arrived");
		J_Event.Set_Field("train_id",Train_ID);
		J_Event.Set_Field("station",Station);
		J_Event.Set_Field("segment",Segment);
		J_Event.Set_Field("platform",Platform);
		J_Event.Set_Field("time",Time);
		J_Event.Set_Field("delay",Train_Delay);

		Send_Event(J_Event.Write);

    end Set_Train_Arrived_Status;


    procedure Set_Train_Left_Status(
		Train		: Positive;
		Station		: String;
		Time		: Positive;
		Segment		: Positive)
	is
		J_Event : JSON_Value := Create_Object;
	begin
		J_Event.Set_Field("type","train_left");
		J_Event.Set_Field("time",Time);
		J_Event.Set_Field("segment",Segment);
		J_Event.Set_Field("train_id",Train);
		J_Event.Set_Field("station",Station);

		Send_Event(J_Event.Write);

	end Set_Train_Left_Status;

	procedure Notify_Termination is
		Parameters : YAMI.Parameters.Parameters_Collection := YAMI.Parameters.Make_Parameters;
	begin
		Parameters.Set_String("node_name",Environment.Get_Node_Name);

		Message_Agent.Instance.Send(
			Destination_Address => Environment.Get_Central_Controller,
			Object 				=> "central_controller",
			Service 			=> "node_terminated",
			Params 				=> Parameters,
			Callback 			=> null);

    end Notify_Termination;

    -- # ####################################### TRAVELER ###############################

	procedure Set_Traveler_Entering_Status(
		Traveler	: Positive;
		Train		: Positive;
		Station		: String;
		Platform	: Positive)
	is
		J_Event : JSON_Value := Create_Object;
	begin
		J_Event.Set_Field("type","traveler_event");
		J_Event.Set_Field("traveler_id",Environment.Travelers(Traveler).Traveler.Id);
		J_Event.Set_Field("train_id",Train);
		J_Event.Set_Field("station",Station);
		J_Event.Set_Field("platform",Platform);
		J_Event.Set_Field("action","enter");
		Central_Controller_Interface.Send_Event(J_Event.Write);
    end Set_Traveler_Entering_Status;

	procedure Set_Traveler_Left_Status(
		Traveler	: Positive;
		Train		: Positive;
		Station		: String;
		Platform	: Positive)
	is
		J_Event : JSON_Value := Create_Object;
	begin
		J_Event.Set_Field("type","traveler_event");
		J_Event.Set_Field("traveler_id",Environment.Travelers(Traveler).Traveler.Id);
		J_Event.Set_Field("name",Environment.Travelers(Traveler).Traveler.Name);
		J_Event.Set_Field("surname",Environment.Travelers(Traveler).Traveler.Surname);
		J_Event.Set_Field("train_id",Train);
		J_Event.Set_Field("station",Station);
		J_Event.Set_Field("platform",Platform);
		J_Event.Set_Field("action","leave");

		Central_Controller_Interface.Send_Event(J_Event.Write);
    end Set_Traveler_Left_Status;

	procedure Set_Traveler_Finished_Status(
		Traveler	: Positive;
		Train		: Positive;
		Station		: String;
		Platform	: Positive)
	is
		J_Event : JSON_Value := Create_Object;
	begin
		J_Event.Set_Field("type","traveler_event");
		J_Event.Set_Field("traveler_id",Traveler);
		J_Event.Set_Field("train_id",Train);
		J_Event.Set_Field("station",Station);
		J_Event.Set_Field("platform",Platform);
		J_Event.Set_Field("action","finished");

		Central_Controller_Interface.Send_Event(J_Event.Write);
    end Set_Traveler_Finished_Status;


	procedure Set_Traveler_Buying_Status(
		Traveler	: Positive;
		Station		: String)
	is
		J_Event : JSON_Value := Create_Object;
	begin
		J_Event.Set_Field("type","traveler_event");
		J_Event.Set_Field("traveler_id",Traveler);
		J_Event.Set_Field("station",Station);
		J_Event.Set_Field("action","buy");

		Central_Controller_Interface.Send_Event(J_Event.Write);
	end Set_Traveler_Buying_Status;

end Central_Controller_Interface;