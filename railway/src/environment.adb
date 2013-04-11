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
with JSON_Helper;use JSON_Helper;
with Logger;
with Gnatcoll.JSON;use Gnatcoll.JSON;

package body Environment Is

   	function Get_Node_Name return String is
   	begin
   		return To_String(Node_Name);
    end Get_Node_Name;

	function Get_Name_Server return String is
	begin
		return To_String(Name_Server);
    end Get_Name_Server;


	function Get_Station_Array(Json_Station : String) return Generic_Station.Stations_Array_Ref is
		Json_v  : Json_Value := Get_Json_Value(Json_File_Name => Json_Station);
		J_Array : constant JSON_Array := Json_v.Get(Field => "stations");
		Array_Length : constant Natural := Length (J_Array);
		T : Generic_Station.Stations_Array_Ref := new Generic_Station.Stations_Array(1 .. Array_Length);
	begin
		for I in 1 .. T'Length loop
			declare
				Json_S : JSON_Value := Get(Arr => J_Array, Index => I);
				Station_Type : String := Json_S.Get("type");
			begin
				if Station_Type = "R" then
					T(I) := Regional_Station.Get_Regional_Station(Get(Arr => J_Array, Index => I));
				else
					T(I) := Gateway_Station.Get_Gateway_Station(Get(Arr => J_Array, Index => I));
				end if;
			end;

		end loop;

		return T;
    end Get_Station_Array;



    procedure Init(
 		N_N 		: in String;
    	N_S 		: in String)
    is
    begin
    	Name_Server := To_Unbounded_String(N_S);

		Node_Name := To_Unbounded_String(N_N);

    	-- # Creates regional stations array loading data from file
    	Stations 	:= Get_Station_Array("res/" & To_String(Node_Name) & "-stations.json");

    	Put_Line("There are " & Integer'Image(Stations'Length) & " stations");

		-- # Creates travelers array loading data from file
    	Travelers 	:= Traveler.Get_Traveler_Manager_array("res/travelers.json");

		Travelers(1).Ticket := Ticket.Get_Ticket(JSON_Helper.Load_File("res/tickets.json"));

		-- # Create an operations set for each Traveler
    	Operations	:= new Traveler.Travelers_All_Operations(1 .. Travelers'Length);

		for I in 1 .. Operations'Length loop
			Operations(I) := new Traveler.Traveler_Operations(Traveler.BUY_TICKET .. Traveler.TICKET_READY);
			Operations(I)(Traveler.BUY_TICKET) := new Move_Operation.Buy_Ticket_Operation_Type'(Traveler_Manager_Index => I);
			Operations(I)(Traveler.LEAVE) := new Move_Operation.Leave_Operation_Type'(Traveler_Manager_Index => I);
			Operations(I)(Traveler.ENTER) := new Move_Operation.Enter_Operation_Type'(Traveler_Manager_Index => I);
			Operations(I)(Traveler.TICKET_READY) := new Move_Operation.Ticket_Ready_Operation_Type'(Traveler_Manager_Index => I);
		end loop;


    end Init;

    procedure Update_Traveler(
		Traveler_Index	: in 		Positive;
		Trav_To_Copy 	: in		Traveler.Traveler_Manager;
		Ticket_To_Copy 	: access 	Ticket.Ticket_Type) is
	begin
		if Traveler_Index <= Travelers'Length then
			Travelers(Traveler_Index) := Trav_To_Copy;
			Travelers(Traveler_Index).Ticket := Ticket_To_Copy;
		end if;
    end Update_Traveler;

	function Get_Index_For_Name(
		Name 			: in 	String) return Natural
	is
	begin
		for I in 1 .. Stations'Length loop
			if Stations(I).Get_Name = Name then
				return I;
			end if;
		end loop;
		return 0;
    end Get_Index_For_Name;

end Environment;
