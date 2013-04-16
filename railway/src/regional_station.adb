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

with Ada.Text_IO;use Ada.Text_IO;
with Logger;
with Environment;
with Route;use Route;
with Trains;
with Routes;
with Ada.Exceptions;
with Train;use Train;
with Ticket_Office;

package body Regional_Station is

	-- ------------------------ Definition of the inherited abstract methods ------------------------

	function Get_Name(
			This : in	Regional_Station_Type) return String is
	begin
		return To_String(This.Name);
    end Get_Name;



	procedure Enter(
			This 				: in		Regional_Station_Type;
			Descriptor_Index	: in		Positive;
			Platform_Index		: in		Positive;
			Segment_ID			: in 		Positive;
			Action				: in 		Route.Action) is
	begin
		-- # First, pass the Access Controller, to maintain the same order
		-- # as the exit order from the Segment <Segment_ID>
		This.Segments_Map_Order.Element(Segment_ID).Enter(
			Train_Index	=> Descriptor_Index);

		-- # Once the task passed the Access controller, it occupies the Platform and,
		-- # if needed, performs Alighting of Passengers.
		This.Platforms(Platform_Index).Enter(
			Train_Descriptor_Index 	=> Descriptor_Index,
			Action					=> Action);


		-- # Frees the Access controller, to let other Tasks to be awaked.
		This.Segments_Map_Order.Element(Segment_ID).Free;

		This.Panel.Set_Status(
			"Train " & Integer'Image(Trains.Trains(Descriptor_Index).ID) & " gained access to Platform " &
			Integer'Image(Platform_Index)
		);
	end Enter;



	procedure Leave(
			This 				: in 		Regional_Station_Type;
			Descriptor_Index	: in		Positive;
			Platform_Index		: in		Positive;
			Action				: in 		Route.Action) is
	begin

		-- # Free the Current Platform and perform boarding of the Travelers.
		This.Platforms(Platform_Index).Leave(
			Train_Descriptor_Index 	=> Descriptor_Index,
			Action					=> Action);

		This.Panel.Set_Status(
			"Train " & Integer'Image(Trains.Trains(Descriptor_Index).ID) & " leaved Platform " &
			Integer'Image(Platform_Index)
		);
	end Leave;

	-- #
	-- # Procedure called by a Traveler to enqueue at a given Platform
	-- # waiting for a specific Train
	-- #
	procedure Wait_For_Train_To_Go(
			This 				: in		Regional_Station_Type;
			Outgoing_Traveler 	: in		Positive;
			Train_ID 			: in		Positive;
			Platform_Index		: in		Positive) is
	begin
		This.Platforms(Platform_Index).Add_Outgoing_Traveler(Outgoing_Traveler);
		This.Panel.Set_Status(
			"Traveler " & Traveler.Get_Name(Environment.Travelers(Outgoing_Traveler)) &
			" waits by platform " & Integer'Image(Platform_Index) & " to GO");
	end Wait_For_Train_To_Go;



	overriding procedure Wait_For_Train_To_Arrive(
			This 				: in		Regional_Station_Type;
			Incoming_Traveler 	: in		Positive;
			Train_ID 			: in		Positive;
			Platform_Index		: in		Positive) is
	begin
		This.Platforms(Platform_Index).Add_Incoming_Traveler(Incoming_Traveler);
		This.Panel.Set_Status(
			"Traveler " & Traveler.Get_Name(Environment.Travelers(Incoming_Traveler)) &
			" waits by platform " & Integer'Image(Platform_Index) & " to ARRIVE");
    end Wait_For_Train_To_Arrive;


	overriding procedure Add_Train(
			This				: in 		Regional_Station_Type;
			Train_ID			: in 		Positive;
			Segment_ID			: in 		Positive) is
	begin
		if not This.Segments_Map_Order.Contains(Segment_ID) then
			Logger.Log(
				Sender 	=> "Regional_Station",
				Message => "Created List for Segment " & Integer'Image(Segment_ID),
				L 		=> Logger.DEBUG
			);
			declare
				-- # Create a new Access Controller for the Segment.
				R : Access_Controller_Ref := new Access_Controller;
			begin
				This.Segments_Map_Order.Insert(
					Key 		=> Segment_ID,
					New_Item 	=> R);
			end;
		end if;
		Logger.Log(
			Sender 	=> "Regional_Station",
			Message => "Adding Train " & Integer'Image(Train_ID),
			L 		=> Logger.DEBUG
		);
		This.Segments_Map_Order.Element(Segment_ID).Add_Train(Trains.Trains(Train_ID).ID);
		This.Panel.Set_Status(
			"Train " & Integer'Image(Trains.Trains(Train_ID).ID) &
			" arriving at platform " &
			Integer'Image(
				Routes.All_Routes(Trains.Trains(Train_ID).Route_Index)(Trains.Trains(Train_ID).Next_Stage).Platform_Index
			)
		);
    end Add_Train;


	procedure Buy_Ticket(
		This 			: in	 Regional_Station_Type;
		Traveler_Index	: in	 Positive;
		To				: in 	 String)
	is
	begin
		Ticket_Office.Get_Ticket(Traveler_Index,This.Get_Name,To);
	end Buy_Ticket;



    protected body Access_Controller is

		entry Enter(
			Train_Index : in 	 Positive) when True is
		begin

			-- # If the currently running Train is the next that can Access the Platform,
			-- # let it pass, otherwise re-queue to Wait entry, until the guard is opened.
			if Trains.Trains(Train_Index).ID /= Trains_Order.Get(1) then
				Logger.Log(
					Sender 	=> "Access_Controller",
					Message	=> "Train " & Integer'Image(Train_Index) & " cannot enter, it is not" &
					  				Integer'Image(Trains_Order.Get(1)),
					L 		=> Logger.DEBUG
				);
				requeue Wait;
			end if;

		end Enter;


		entry Wait(
			Train_Index	: in 	 Positive) when Can_Retry
		is
		begin
			-- # Decrease the number of re-attempting Trains
			Trains_Waiting := Trains_Waiting - 1;

			-- # If the number is 0, then close the guard.
			if Trains_Waiting = 0 then
				Can_Retry := False;
			end if;

			-- # If the currently running Train is the next that can Access the Platform,
			-- # let it pass, otherwise re-queue to Wait entry, until the guard is opened.
			if Trains.Trains(Train_Index).ID /= Trains_Order.Get(1) then
				requeue Wait;
			end if;
		end Wait;


		procedure Free is
			Train_ID	: Positive;
		begin
			-- # Dequeue the First Train (the one which already called Enter).
			Trains_Order.Dequeue(Train_ID);

			-- # Check if the number of waiting Trains in Wait entry is > 0.
			if Wait'Count >0 then
				-- # Open the guard to let them retry
				Trains_Waiting := Wait'Count;
				Can_Retry := True;
				Logger.Log(
					Sender 	=> "Access_Controller",
					Message	=> "Train " & Integer'Image(Train_ID) & " opened the guard Wait",
					L 		=> Logger.DEBUG
				);
			end if;
		end Free;

		procedure Add_Train(
			Train_ID : in 	 Positive) is
		begin
			-- # Add a new element in the Queue.
			Trains_Order.Enqueue(Train_ID);
		end Add_Train;

 	end Access_Controller;




	function New_Regional_Station(
			Platforms_Number 	: in		Positive;
			Name 				: in 		String) return Station_Ref
	is
		Station : access Regional_Station_Type := new Regional_Station_Type(Platforms_Number);
	begin
		Station.Name := Unbounded_Strings.To_Unbounded_String(Name);
		for I in Positive range 1..Platforms_Number loop
			Station.Platforms(I) := new Platform.Platform_Handler(I,Station.Name'Access);
		end loop;
		Station.Panel := new Notice_Panel.Notice_Panel_Entity(new String'(To_String(Station.Name)));
		return Station;
	end;




	overriding procedure Print(This : in Regional_Station_Type) is
	begin
		Put_Line ("Name : " & Unbounded_Strings.To_String(This.Name));
		Put_Line ("Platform Number : " & Integer'Image(This.Platforms_Number));
    end Print;


-- ################################################ JSON - Regional Station ##########################################

	function Get_Regional_Station(Json_Station : Json_Value) return Station_Ref
	is
		Platforms_Number : Positive := Json_Station.Get("platform_number");
		Name : String				:= Json_Station.Get("name");
	begin
		return New_Regional_Station(Platforms_Number,Name);
	end Get_Regional_Station;


	function Get_Regional_Station_Array(Json_Station : String) return Stations_Array_Ref is
		Json_v  : Json_Value := Get_Json_Value(Json_File_Name => Json_Station);
		J_Array : constant JSON_Array := Json_v.Get(Field => "stations");
		Array_Length : constant Natural := Length (J_Array);
		T : Stations_Array_Ref := new Stations_Array(1 .. Array_Length);
	begin
		for I in 1 .. T'Length loop
			T(I) := Get_Regional_Station(Get(Arr => J_Array, Index => I));
		end loop;

		return T;
    end Get_Regional_Station_Array;


    overriding procedure Finalize (This: in out Regional_Station_Type) is
    begin
    	Logger.Log(
    		Sender => "Regional_Station",
    		Message => "Finalize Station " & Unbounded_Strings.To_String(This.Name),
    		L => Logger.DEBUG);
    end Finalize;

end Regional_Station;
