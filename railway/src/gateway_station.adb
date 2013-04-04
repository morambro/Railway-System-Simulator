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

package body Gateway_Station is

	-- ------------------------ Definition of the inherited abstract methods ------------------------
	procedure Enter(
			This 				: in		Gateway_Station_Type;
			Descriptor_Index	: in		Positive;
			Platform_Index		: in		Positive;
			Action				: in 		Route.Action) is
	begin

		if Action = Route.ENTER then
			This.Platforms(Platform_Index).Enter(Descriptor_Index);
		end if;
	end Enter;



	procedure Send_Train(
		Train_D 		: in	 Positive;
		Station	 		: in	 Positive;
		Platform		: in 	 Positive;
		Node_Name		: in	 String )
	is
		Parameters : YAMI.Parameters.Parameters_Collection := YAMI.Parameters.Make_Parameters;
	begin

		-- # First, Resolve destination address
		declare

			-- # Callback function, used to handle the response
			procedure Get_Results(Content : in out YAMI.Parameters.Parameters_Collection) is

				Address : String := Content.Get_String("response");
				Parameters : YAMI.Parameters.Parameters_Collection := YAMI.Parameters.Make_Parameters;

			begin
				Logger.Log(
					Sender 	=> "Gateway_Station",
					Message => "The destination is located at address " & Address,
					L 		=> Logger.DEBUG
				);

				if Address /= "_" then
					-- # Once the address is resolved, send the Train to the destination node, and interrupt
					-- # the current task Train execution.

					Parameters.Set_String("station",Integer'Image(Station));
					Parameters.Set_String("platform",Integer'Image(Platform));
					Parameters.Set_String("train_index",Integer'Image(Train_D));
					Parameters.Set_String("train",Train.Get_Json(Trains.Trains(Train_D)));

					Message_Agent.Instance.Send(
						Destination_Address => Address,
						Object 				=> "message_handler",
						Service 			=> "train_transfer",
						Params 				=> Parameters,
						Callback			=> null
					);

					Put_Line ("NEXT STAGE = " & Integer'Image(Trains.Trains(Train_D).Next_Stage));
				end if;

		    end Get_Results;

		begin

			Parameters.Set_String("station",Integer'Image(Station));
			Parameters.Set_String("node_name",Node_Name);

			Put_Line(Environment.Get_Name_Server);

			Message_Agent.Instance.Send(
				Destination_Address => Environment.Get_Name_Server,
				Object 				=> "name_server",
				Service 			=> "get",
				Params 				=> Parameters,
				Callback			=> Get_Results'Access
			);

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



	procedure Leave(
			This 				: in 		Gateway_Station_Type;
			Descriptor_Index	: in		Positive;
			Platform_Index		: in		Positive) is
	begin
		-- # Send train with descriptor Train_D to the next Station, if defined and on another node, and stop current Train Execution

		Put_Line("Next_Stage = " & Integer'Image(Trains.Trains(Descriptor_Index).Next_Stage));

		if Trains.Trains(Descriptor_Index).Next_Stage + 1 <= Routes.All_Routes(Trains.Trains(Descriptor_Index).Route_Index)'Length then

			Trains.Trains(Descriptor_Index).Next_Stage := Trains.Trains(Descriptor_Index).Next_Stage + 1;

			declare

				-- # Retrieve Next Station index
				Next_Station_Index 	: Positive 	:=
					Routes.All_Routes(Trains.Trains(Descriptor_Index).Route_Index)(Trains.Trains(Descriptor_Index).Next_Stage).Next_Station;
				-- # Retrieve the Node_Name
				Next_Station_Node	: String 	:= To_String(
					Routes.All_Routes(Trains.Trains(Descriptor_Index).Route_Index)(Trains.Trains(Descriptor_Index).Next_Stage).Node_Name);

			begin

				if  Next_Station_Node/= Environment.Get_Node_Name then
					-- #
					-- #
					-- #
					-- # SEND to the next Station TODO!!!!
					-- #
					-- #
					-- #
					Put_Line("DEST_REGION : " & Next_Station_Node & " NEXT STATION INDEX " & Integer'Image(Next_Station_Index));
					Send_Train(
						Train_D		=>	Descriptor_Index,
						Station 	=> 	Next_Station_Index,
						-- # The platform index will be the same!!
						Platform	=>	Platform_Index,
						Node_Name 	=>	Next_Station_Node);
				else
					-- # If the next Stage is in the same Region, proceed with a local Leave operation
					This.Platforms(Platform_Index).Leave(Descriptor_Index);
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
			Platform_Index		: in		Positive) is
	begin
		This.Platforms(Platform_Index).Add_Outgoing_Traveler(Outgoing_Traveler);
		This.Panel.SetStatus(
			"Traveler " & Traveler.Get_Name(Environment.Get_Travelers(Outgoing_Traveler)) &
			" waits by platform " & Integer'Image(Platform_Index) & " station " &
			Unbounded_Strings.To_String(This.Name) & " to GO");
	end Wait_For_Train_To_Go;



	overriding procedure Wait_For_Train_To_Arrive(
			This 				: in		Gateway_Station_Type;
			Incoming_Traveler 	: in		Positive;
			Train_ID 			: in		Positive;
			Platform_Index		: in		Positive) is
	begin
		This.Platforms(Platform_Index).Add_Incoming_Traveler(Incoming_Traveler);
		This.Panel.SetStatus(
			"Traveler " & Traveler.Get_Name(Environment.Get_Travelers(Incoming_Traveler)) &
			" waits by station " & Unbounded_Strings.To_String(This.Name)
			& " at platform " & Integer'Image(Platform_Index) & " to ARRIVE");
    end Wait_For_Train_To_Arrive;


	overriding procedure Add_Train(
			This				: in 		Gateway_Station_Type;
			Train_ID			: in 		Positive;
			Segment_ID			: in 		Positive) is
	begin
		null;
    end Add_Train;



	function New_Gateway_Station(
			Platforms_Number 	: in		Positive;
			Name 				: in 		String) return Station_Ref
	is
		Station : access Gateway_Station_Type := new Gateway_Station_Type(Platforms_Number);
	begin
		Station.Name := Unbounded_Strings.To_Unbounded_String(Name);
		for I in Positive range 1..Platforms_Number loop
			Station.Platforms(I) := new Gateway_Platform.Gateway_Platform_Type(I,Station.Name'Access);
		end loop;
		Station.Panel := new Notice_Panel.Notice_Panel_Entity(1);
		return Station;
	end;


	overriding procedure Print(This : in Gateway_Station_Type) is
	begin
		Put_Line ("Name : " & Unbounded_Strings.To_String(This.Name));
		Put_Line ("Platform Number : " & Integer'Image(This.Platforms_Number));
    end Print;


    function Get_Platform(This : Gateway_Station_Type;P : Natural) return Generic_Platform.Platform_Access is
    begin
    	return This.Platforms(P);
    end Get_Platform;

    overriding procedure Finalize (This: in out Gateway_Station_Type) is
    begin
    	Logger.Log(
    		Sender => "Gateway_Station",
    		Message => "Finalize Station " & Unbounded_Strings.To_String(This.Name),
    		L => Logger.DEBUG);
    end Finalize;

-- ########################################### JSON - Gateway Station ##########################################

	function Get_Gateway_Station(Json_Station : Json_Value) return Station_Ref
	is
		Platforms_Number : Positive := Json_Station.Get("platform_number");
		Name : String				:= Json_Station.Get("name");
	begin
		return New_Gateway_Station(Platforms_Number,Name);
	end Get_Gateway_Station;


--  	function Get_Regional_Station_Array(Json_Station : String) return Stations_Array_Ref is
--  		Json_v  : Json_Value := Get_Json_Value(Json_File_Name => Json_Station);
--  		J_Array : constant JSON_Array := Json_v.Get(Field => "stations");
--  		Array_Length : constant Natural := Length (J_Array);
--  		T : Stations_Array_Ref := new Stations_Array(1 .. Array_Length);
--  	begin
--  		for I in 1 .. T'Length loop
--  			T(I) := Get_Regional_Station(Get(Arr => J_Array, Index => I));
--  		end loop;
--
--  		return T;
--      end Get_Regional_Station_Array;


end Gateway_Station;
