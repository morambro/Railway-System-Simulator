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
with Routes;
with Trains;
with YAMI.Parameters;
with Message_Agent;
with Ada.Exceptions;
with Ticket;

package body Gateway_Station is

	-- ########################################## ACCESS_CONTROL #############################################
	overriding procedure Add_Train(
		This				: in 		Gateway_Station_Type;
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
				R : Access_Controller_Ref := new Access_Controller(This.Platforms);
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
    end Add_Train;

	protected body Access_Controller is

		entry Enter(
			Train_ID 	: in 	Positive;
			Action 		: in	Route.Action) when True
		is
			T	: Positive;
		begin
			if Trains.Trains(Train_ID).ID = Trains_Order.Get(1) then
				-- # Dequeue
				Trains_Order.Dequeue(T);

				-- # If some task is waiting on Wait entry, open the guard
				if Wait'Count > 0 then
					Can_Retry := True;
				end if;

				requeue Platforms(
					Routes.All_Routes(Trains.Trains(Train_ID).Route_Index)(Trains.Trains(Train_ID).Next_Stage)
						.Platform_Index
					).Enter;
			else
				Can_Retry := False;
				Logger.Log(
					Sender 	=> "Access_Controller",
					Message	=> "Train " & Integer'Image(Trains.Trains(Train_ID).ID) & " cannot enter, it is not" &
					  				Integer'Image(Trains_Order.Get(1)),
					L 		=> Logger.DEBUG
				);
				requeue Wait;
			end if;

		end Enter;


		entry Wait(
			Train_ID 	: in 	Positive;
			Action 		: in	Route.Action) when Can_Retry
		is
			T	: Positive;
		begin
			if Trains.Trains(Train_ID).ID = Trains_Order.Get(1) then
				-- # Dequeue
				Trains_Order.Dequeue(T);

				-- # If some task is waiting on Wait entry, open the guard
				if Wait'Count > 0 then
					Can_Retry := True;
				end if;

				requeue  Platforms(
					Routes.All_Routes(Trains.Trains(Train_ID).Route_Index)(Trains.Trains(Train_ID).Next_Stage).Platform_Index
				).Enter;
			else
				Can_Retry := False;
				requeue Wait;
			end if;
		end Wait;

		procedure Add_Train(
			Train_ID : in 	 Positive) is
		begin
			Trains_Order.Enqueue(Train_ID);
		end Add_Train;

 	end Access_Controller;


	-- ###################################################################################################
	-- ######################### Definition of the inherited abstract methods ############################
	-- ###################################################################################################

	procedure Enter(
			This 				: in		Gateway_Station_Type;
			Descriptor_Index	: in		Positive;
			Platform_Index		: in		Positive;
			Segment_ID			: in 		Positive;
			Action				: in 		Route.Action)
	is
	begin

		This.Segments_Map_Order.Element(Segment_ID).Enter(
			Train_ID 	=> Descriptor_Index,
			Action		=> Action);
		This.Panel.Set_Status(
			"Train " & Integer'Image(Trains.Trains(Descriptor_Index).ID) & " gained access to Platform " &
			Integer'Image(Platform_Index)
		);

		-- # If we have to cross to other region, we are positioned a stage before the first stage on the next region.
		-- # So, let's check for Trains.Trains(Descriptor_Index).Next_Stage + 1!
		if 	Trains.Trains(Descriptor_Index).Next_Stage + 1 <= Routes.All_Routes(Trains.Trains(Descriptor_Index).Route_Index)'Length then
			declare
				-- # Retrieve Next Station index
				Next_Station_Index 	: Positive 	:=
					Routes.All_Routes(Trains.Trains(Descriptor_Index).Route_Index)(Trains.Trains(Descriptor_Index).Next_Stage + 1).Start_Station;
				-- # Retrieve the Node_Name
				Next_Station_Node	: String 	:= To_String(
					Routes.All_Routes(Trains.Trains(Descriptor_Index).Route_Index)(Trains.Trains(Descriptor_Index).Next_Stage + 1).Node_Name);

			begin
			  	if Next_Station_Node /= Environment.Get_Node_Name then
				-- # Send train with descriptor Train_D to the next Station, if defined and on another node, and stop current Train Execution

					-- # Go to the next stage, to let the train start to the right position on the next region
					Trains.Trains(Descriptor_Index).Next_Stage := Trains.Trains(Descriptor_Index).Next_Stage + 1;

					This.Send_Train(
						Train_Descriptor_Index		=>	Descriptor_Index,
						Station 					=> 	Next_Station_Index,
						-- # The platform index will be the same!!
						Platform					=>	Platform_Index,
						Node_Name 					=>	Next_Station_Node);
				end if;
			end;
		end if;

	end Enter;


	procedure Leave(
			This 				: in 		Gateway_Station_Type;
			Descriptor_Index	: in		Positive;
			Platform_Index		: in		Positive;
			Action				: in 		Route.Action)
	is

	begin
		-- # If the next Stage is in the same Region, proceed with a local Leave operation
		This.Platforms(Platform_Index).Leave(
			Train_Descriptor_Index 	=> Descriptor_Index,
			Action						=> Action);
		This.Panel.Set_Status(
			"Train " & Integer'Image(Trains.Trains(Descriptor_Index).ID) & " leaved Platform " &
			Integer'Image(Platform_Index)
		);

		if Trains.Trains(Descriptor_Index).Next_Stage - 1 > 0 then
			declare
				-- # Retrieve previous Station index
				Previous_Station_Index 	: Positive 	:=
					Routes.All_Routes(Trains.Trains(Descriptor_Index).Route_Index)(Trains.Trains(Descriptor_Index).Next_Stage - 1).Next_Station;
				-- # Retrieve previous Node
				Previous_Station_Node	: String 	:= To_String(
					Routes.All_Routes(Trains.Trains(Descriptor_Index).Route_Index)(Trains.Trains(Descriptor_Index).Next_Stage - 1).Node_Name);

			begin
				if Previous_Station_Node /= Environment.Get_Node_Name then
					-- # The train was sent by remote Region. So notify it left the Platform!
					This.Send_Ack(
						Train_Descriptor_Index		=>	Descriptor_Index,
						Station 					=> 	Previous_Station_Index,
						-- # The platform index were the same!!
						Platform					=>	Platform_Index,
						Node_Name 					=>	Previous_Station_Node);
				end if;
			end;
		end if;

	end Leave;



	-- #
	-- # Procedure called by a Traveler to enqueue at a given Platform
	-- # waiting for a specific Train
	-- #
	procedure Wait_For_Train_To_Go(
			This 				: in		Gateway_Station_Type;
			Outgoing_Traveler 	: in		Positive;
			Train_ID 			: in		Positive;
			Platform_Index		: in		Positive)
	is
		Next_Stage_Region : Unbounded_String :=
			Environment.Travelers(Outgoing_Traveler).Ticket.Stages(
				Environment.Travelers(Outgoing_Traveler).Ticket.Next_Stage).Region;
	begin
		if Next_Stage_Region /= Environment.Get_Node_Name then

			declare
				Next_Station_Index : Positive := This.Destinations.Element(To_String(Next_Stage_Region));
			begin
				Send_Traveler_To_Leave (
					Traveler_Index	=> Outgoing_Traveler,
					Train_ID 		=> Train_ID,
					Station 		=> Next_Station_Index,
					Platform 		=> Platform_Index,
					Node_Name		=> To_String(Next_Stage_Region)
				);
			end;

		else
			This.Platforms(Platform_Index).Add_Outgoing_Traveler(Outgoing_Traveler);
			This.Panel.Set_Status(
				"Traveler " & Traveler.Get_Name(Environment.Travelers(Outgoing_Traveler)) &
				" waits by platform " & Integer'Image(Platform_Index) & " station " &
				Unbounded_Strings.To_String(This.Name) & " to GO");
		end if;
	end Wait_For_Train_To_Go;



	overriding procedure Wait_For_Train_To_Arrive(
			This 				: in		Gateway_Station_Type;
			Incoming_Traveler 	: in		Positive;
			Train_ID 			: in		Positive;
			Platform_Index		: in		Positive) is
	begin
		This.Platforms(Platform_Index).Add_Incoming_Traveler(Incoming_Traveler);
		This.Panel.Set_Status(
			"Traveler " & Traveler.Get_Name(Environment.Travelers(Incoming_Traveler)) &
			" waits by station " & Unbounded_Strings.To_String(This.Name)
			& " at platform " & Integer'Image(Platform_Index) & " to ARRIVE");
    end Wait_For_Train_To_Arrive;


    procedure Occupy_Platform(
			This					: in 	 Gateway_Station_Type;
			Platform_Index			: in 	 Positive;
			Train_Index				: in 	 Positive) is
	begin
		This.Platforms(Platform_Index).Enter(Train_Index,Route.FREE);
    end Occupy_Platform;


	-- ############################### REMOTE SEND PROCEDURES #########################################

	procedure Send_Train(
		This					: in 	 Gateway_Station_Type;
		Train_Descriptor_Index 	: in	 Positive;
		Station	 				: in	 Positive;
		Platform				: in 	 Positive;
		Node_Name				: in	 String )
	is
		Parameters : YAMI.Parameters.Parameters_Collection := YAMI.Parameters.Make_Parameters;
	begin
		-- # First, Resolve destination address
		declare


			procedure Send (
				Address 	: in	String)
			is
				Parameters : YAMI.Parameters.Parameters_Collection := YAMI.Parameters.Make_Parameters;
			begin
				Logger.Log(
					Sender 	=> "Gateway_Station",
					Message => "The destination is located at address " & Address,
					L 		=> Logger.DEBUG
				);

				-- # Once the address is resolved, send the Train to the destination node, and interrupt
				-- # the current task Train execution.

				Parameters.Set_String("station",Integer'Image(Station));
				Parameters.Set_String("platform",Integer'Image(Platform));
				Parameters.Set_String("train_index",Integer'Image(Train_Descriptor_Index));
				Parameters.Set_String("train",Train.Get_Json(Trains.Trains(Train_Descriptor_Index)));

				Message_Agent.Instance.Send(
					Destination_Address => Address,
					Object 				=> "message_handler",
					Service 			=> "train_transfer",
					Params 				=> Parameters,
					Callback			=> null
				);

	 	  	end Send;

			-- # Callback function, used to handle the response
			procedure Get_Results(Content : in out YAMI.Parameters.Parameters_Collection) is
				Address : String := Content.Get_String("response");

			begin
				if Address /= "_" then
					-- # Add the found Address to the List
					Last_Addresses.Insert(Node_Name,Address);
					Send(Address);
				else
					Logger.Log(
						Sender 		=> "Gateway_Station.Send_Train",
						Message		=> "Unable to locate " & Node_Name,
						L			=> Logger.ERROR);
				end if;
		    end Get_Results;

		begin

			if Last_Addresses.Contains(Node_Name) then
				Send(Last_Addresses.Element(Node_Name));
			else
				Parameters.Set_String("node_name",Node_Name);

				Message_Agent.Instance.Send(
					Destination_Address => Environment.Get_Name_Server,
					Object 				=> "name_server",
					Service 			=> "get",
					Params 				=> Parameters,
					Callback			=> Get_Results'Access
				);
			end if;

		exception
			when E : others =>
				Logger.Log(
	   				Sender => "Gateway_Station.Send_Train",
	   				Message => "ERROR : Exception: " & Ada.Exceptions.Exception_Name(E) & "  " & Ada.Exceptions.Exception_Message(E),
	   				L => Logger.ERROR);
		end;

		-- # Stop current execution raising an exception.
		raise Gateway_Platform.Stop_Train_Execution;

    end Send_Train;


	procedure Send_Ack(
		This					: in 	 Gateway_Station_Type;
		Train_Descriptor_Index 	: in	 Positive;
		Station	 				: in	 Positive;
		Platform				: in 	 Positive;
		Node_Name				: in	 String )
	is
		Parameters : YAMI.Parameters.Parameters_Collection := YAMI.Parameters.Make_Parameters;
	begin

		-- # First, Resolve destination address
		declare

			procedure Send(
				Address 	: in	String)
			is
				Parameters : YAMI.Parameters.Parameters_Collection := YAMI.Parameters.Make_Parameters;

			begin
				Logger.Log(
					Sender 	=> "Gateway_Station.Send_Ack",
					Message => "The destination is located at address " & Address,
					L 		=> Logger.DEBUG
				);


					-- # Once the address is resolved, send the Train to the destination node, and interrupt
					-- # the current task Train execution.

					Parameters.Set_String("station",Integer'Image(Station));
					Parameters.Set_String("platform",Integer'Image(Platform));
					Parameters.Set_String("train_index",Integer'Image(Train_Descriptor_Index));

					Message_Agent.Instance.Send(
						Destination_Address => Address,
						Object 				=> "message_handler",
						Service 			=> "train_transfer_ack",
						Params 				=> Parameters,
						Callback			=> null
					);

		    end Send;

			-- # Callback function, used to handle the response
			procedure Get_Results(Content : in out YAMI.Parameters.Parameters_Collection) is

				Address : String := Content.Get_String("response");
			begin
				if Address /= "_" then
					-- # Add the found Address to the List
					Last_Addresses.Insert(Node_Name,Address);
					Send(Address);
				else
					Logger.Log(
						Sender 		=> "Gateway_Station.Send_Train",
						Message		=> "Unable to locate " & Node_Name,
						L			=> Logger.ERROR);
				end if;
			end Get_Results;



		begin
			if Last_Addresses.Contains(Node_Name) then
				Send(Last_Addresses.Element(Node_Name));
			else
				Parameters.Set_String("node_name",Node_Name);

				Message_Agent.Instance.Send(
					Destination_Address => Environment.Get_Name_Server,
					Object 				=> "name_server",
					Service 			=> "get",
					Params 				=> Parameters,
					Callback			=> Get_Results'Access
				);
			end if;

		exception
			when E : others =>
				Logger.Log(
	   				Sender => "Gateway_Station.Send_Train",
	   				Message => "ERROR : Exception: " & Ada.Exceptions.Exception_Name(E) & "  " & Ada.Exceptions.Exception_Message(E),
	   				L => Logger.ERROR);
		end;

    end Send_Ack;


	-- #
	-- # Retrieves the address for the destination Node, then sends the Traveler data
	-- #
    procedure Send_Traveler_To_Leave(
		Traveler_Index	: in	 Positive;
		Train_ID		: in 	 Positive;
		Station	 		: in	 Positive;
		Platform		: in 	 Positive;
		Node_Name		: in	 String )
	is

		procedure Send(
			Address 	: in 	String)
		is
			Parameters : YAMI.Parameters.Parameters_Collection := YAMI.Parameters.Make_Parameters;

		begin
			Logger.Log(
				Sender 	=> "Gateway_Station",
				Message => "The destination is located at address " & Address,
				L 		=> Logger.DEBUG
			);

			if Address /= "_" then

				Parameters.Set_String("station",Integer'Image(Station));
				Parameters.Set_String("platform",Integer'Image(Platform));
				Parameters.Set_String("traveler_index",Integer'Image(Traveler_Index));
				Parameters.Set_String("train_id",Integer'Image(Train_ID));
				Parameters.Set_String("traveler",Traveler.Get_Json(Environment.Travelers(Traveler_Index)));
				Parameters.Set_String("ticket",Ticket.Get_Json(Environment.Travelers(Traveler_Index).Ticket));

				Message_Agent.Instance.Send(
					Destination_Address => Address,
					Object 				=> "message_handler",
					Service 			=> "traveler_leave_transfer",
					Params 				=> Parameters,
					Callback			=> null
				);

			end if;
		end Send;

		-- # Callback function, used to handle the response
		procedure Get_Results(Content : in out YAMI.Parameters.Parameters_Collection) is
			Address : String := Content.Get_String("response");
		begin
			if Address /= "_" then
					-- # Add the found Address to the List
					Last_Addresses.Insert(Node_Name,Address);
					Send(Address);
				else
					Logger.Log(
						Sender 		=> "Gateway_Station.Send_Train",
						Message		=> "Unable to locate " & Node_Name,
						L			=> Logger.ERROR);
				end if;
	    end Get_Results;

		Parameters : YAMI.Parameters.Parameters_Collection := YAMI.Parameters.Make_Parameters;

	begin

		if Last_Addresses.Contains(Node_Name) then
			Send(Last_Addresses.Element(Node_Name));
		else
			Parameters.Set_String("node_name",Node_Name);

			Message_Agent.Instance.Send(
				Destination_Address => Environment.Get_Name_Server,
				Object 				=> "name_server",
				Service 			=> "get",
				Params 				=> Parameters,
				Callback			=> Get_Results'Access
			);
		end if;
	exception
		when E : others =>
			Logger.Log(
   				Sender => "Gateway_Station.Send_Train",
   				Message => "ERROR : Exception: " & Ada.Exceptions.Exception_Name(E) & "  " & Ada.Exceptions.Exception_Message(E),
   				L => Logger.ERROR);


    end Send_Traveler_To_Leave;


    -- ##############################################################################################

	function New_Gateway_Station(
			Platforms_Number 	: in		Positive;
			Name 				: in 		String;
			Destinations		: access String_Positive_Maps.Map) return Station_Ref
	is
		Station : access Gateway_Station_Type := new Gateway_Station_Type(Platforms_Number);
	begin
		Station.Name := Unbounded_Strings.To_Unbounded_String(Name);
		for I in Positive range 1..Platforms_Number loop
			Station.Platforms(I) := new Gateway_Platform.Gateway_Platform_Type(I,Station.Name'Access);
		end loop;
		Station.Panel := new Notice_Panel.Notice_Panel_Entity(new String'(To_String(Station.Name)));
		Station.Destinations := Destinations;
		return Station;
	end;


	overriding procedure Print(This : in Gateway_Station_Type) is
	begin
		Put_Line ("Name : " & Unbounded_Strings.To_String(This.Name));
		Put_Line ("Platform Number : " & Integer'Image(This.Platforms_Number));
    end Print;


    overriding procedure Finalize (This: in out Gateway_Station_Type) is
    begin
    	Logger.Log(
    		Sender => "Gateway_Station",
    		Message => "Finalize Station " & Unbounded_Strings.To_String(This.Name),
    		L => Logger.DEBUG);
    end Finalize;

-- ########################################### JSON - Gateway Station ##########################################

	function Load_Destinations(J_Array : in JSON_Array) return access String_Positive_Maps.Map
	is
		Array_Length : constant Natural := Length (J_Array);
		Map_To_Return : access String_Positive_Maps.Map := new String_Positive_Maps.Map;
	begin

		for I in 1 .. Array_Length loop
			Map_To_Return.Insert(
				Get(Arr => J_Array, Index => I).Get("region"),
				Get(Arr => J_Array, Index => I).Get("station")
			);

		end loop;

		return Map_To_Return;
	end Load_Destinations;

	function Get_Gateway_Station(Json_Station : Json_Value) return Station_Ref
	is
		Platforms_Number 	: Positive := Json_Station.Get("platform_number");
		Name 				: String := Json_Station.Get("name");
		New_Station 		: Station_Ref := New_Gateway_Station(
			Platforms_Number,
			Name,
			Load_Destinations(Json_Station.Get("links")));
	begin
		return New_Station;
	end Get_Gateway_Station;


end Gateway_Station;
