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
with Gnatcoll.JSON;use Gnatcoll.JSON;
with JSON_Helper;use JSON_Helper;
with Ada.Text_IO;use Ada.Text_IO;
with Routes;
with Ada.Exceptions;
with Environment;
with Ada.Strings.Unbounded;use Ada.Strings.Unbounded;
with Route;use Route;
with Task_Pool;
with Traveler;
with Trains;
with Central_Office_Interface;

package body Ticket_Office is


    procedure Init_Path_Map(
		File_Name	: in 	String)
	is
		J_Array 		: JSON_Array := Get_Json_Value(Json_File_Name => File_Name).Get(Field => "paths");
		Array_Length 	: constant Natural := Length(J_Array);
	begin
		for I in 1 .. Array_Length loop
			declare
				M				: String_Array_Map_Ref := new String_Array_Maps.Map;
				From			: Natural := Get(J_Array,I).Get(Field => "from");
				J_Destinations	: JSON_Array := Get(J_Array,I).Get(Field => "destinations");
			begin
				for J in 1 .. Length(J_Destinations) loop
					declare
						J_Path 	: JSON_Array := Get(J_Destinations,J).Get(Field => "path");
						Path 	: Destinations_Ref := new Destinations(1 .. Length(J_Path));
						Dest 	: Natural := Get(J_Destinations,J).Get(Field => "dest");
					begin
						for K in 1 .. Length(J_Path) loop
							Path(k) := Get(J_Path,K).Get;
						end loop;
						M.Insert(
							Key			=> Integer'Image(Dest),
							New_Item 	=> Path);
					end;
				end loop;
				Paths.Insert(
					Key 		=> Integer'Image(From),
					New_Item	=> M);
			end;
		end loop;

    end Init_Path_Map;

	function Create_Ticket(
		From	: in 	String;
		To		: in 	String) return access Ticket.Ticket_Type
	is
		-- # Retrieve the indexes from their names
		S_From 	: String := Integer'Image(Environment.Get_Index_For_Name(From));
		S_To	: String := Integer'Image(Environment.Get_Index_For_Name(To));
	begin
		-- # If both stations are contained in the Paths Map, continue, otherwise return null.
		if Paths.Contains(Key => S_From) and Paths.Element(Key => S_From).Contains(Key => S_To) then
			declare
				-- # Get the best path to reach station S_To from S_From station.
				Best_Path 		: Destinations_Ref := Paths.Element(Key => S_From).Element(Key => S_To);
				I 				: Positive := 1;
				-- # The ticket that will be built
				New_Ticket 		: access Ticket.Ticket_Type := new Ticket.Ticket_Type;
				Stages 			: Ticket.Ticket_Stages(1..Best_Path'Length);
				Stages_Cursor	: Positive := 1;
			begin
				if Best_Path'Length = 1 then
					return null;
				end if;
				while(I < Best_Path'Length) loop
					declare
						-- # Get all the routes that matches with (I,I+1)
						Matches 	: Routes.Routes_Indexes := Routes.Get_Routes_Containing(Best_Path(I),Best_Path(I+1));
						-- # The maximum match length, initially 0
						Max_Length 	: Natural := 0;
						-- # The index of the routes with maximum match length
						Max_Match	: Natural := 0;

						Start_Station 			: Natural := Best_Path(I);
						Next_Station 			: Natural := Best_Path(I);
						Start_Platform  		: Natural := 0;
						Destination_Platform	: Natural := 0;
					begin

						if Matches'Length = 0 then
							raise No_Route_For_Destination with "Cannot create a ticket from " & S_From & " to " & S_To;
						end if;

						-- # Now we have to find the longest match!
						for J in 1 .. Matches'Length loop

							declare
								-- # The current route index from where to start searching for a match
								Start_Index 	: Natural := Routes.Contains(Matches(J),Best_Path(I),Best_Path(I+1));

								-- # Index used to extend the match
								Index : Natural := Start_Index;
								-- # A copy of I to modify
								K 				: Positive := I;
								-- # The Length of the current Match
								Len 			: Natural := 0;
								-- # A Boolean Variable used to stop the loop
								Equals			: Boolean := True;
							begin
								-- # Continue extending the match if and only if Start_Index and K are under their limits, and
								-- # (Best_Path(K),Best_Path(K+1)) is equals to the current route stage

								while 	(Index <= Routes.All_Routes(Matches(J))'Length) and
										(K < Best_Path'Length) and Equals loop

										Equals 	:=	(Best_Path(K+1) = Routes.All_Routes(Matches(J))(Index).Next_Station) and
													(Best_Path(K) 	= Routes.All_Routes(Matches(J))(Index).Start_Station);

										if Equals then
											K := K + 1;
											Len := Len + 1;
											Index := Index + 1;
										end if;
								end loop;
								-- # If this train does not stop at the station, set Len to 0.
								if Routes.All_Routes(Matches(J))(Index-1).Enter_Action /= Route.ENTER then
									Len := 0;
								end if;

								Put_Line("LEN = " & Integer'Image(Len));

								-- # Case in witch we have a new Maximum
								if Len > Max_Length then
									Max_Length := Len;
									Max_Match := J;
									-- # Start Platform for the current examined leg
									Start_Platform := Routes.All_Routes(Matches(J))(Start_Index).Start_Platform;
									-- # The next station of the stage
									Next_Station := Best_Path(K);
									-- # Destination Platform will be the index of the platform found on the last visited route's stage.
									Destination_Platform := Routes.All_Routes(Matches(J))(Index-1).Platform_Index;

								end if;
							end;
						end loop;
						-- # No match case
						if Max_Match = 0 then
							raise No_Route_For_Destination with "Can not create a ticket from " & S_From & " to " & S_To;
						end if;

						-- # Finally create a New Stage of the ticket, with the collected data.
						Stages(Stages_Cursor) := (
							Start_Station 				=> Start_Station,
							Next_Station  				=> Next_Station,
							Train_ID 	  				=> Trains.Train_For_Route(Matches(Max_Match)),
							Start_Platform_Index 		=> Start_Platform,
							-- # The region to which the next stage belongs to
							Region						=> To_Unbounded_String(Environment.Get_Node_Name),
							Destination_Platform_Index	=> Destination_Platform
						);
						Stages_Cursor := Stages_Cursor + 1;
						I := I + Max_Length;
					end;

				end loop;
				-- # Return only the created stages
				New_Ticket.Stages := new Ticket.Ticket_Stages'(Stages(1..Stages_Cursor-1));
				return New_Ticket;
			end;
		end if;
    	return null;
    end Create_Ticket;


	procedure Get_Ticket (
		Traveler_Index 	: in 	Positive;
		From			: in 	String;
		To				: in 	String)
	is
	begin
		-- # If the Stations are contained in the current Region, create the ticket and return it directly.
		Put_Line("CREATE : " & integer'image(Environment.Get_Index_For_Name(From)) & " to " & integer'image(Environment.Get_Index_For_Name(To)));

		if Environment.Get_Index_For_Name(From) /= 0 and Environment.Get_Index_For_Name(To) /= 0 then

			-- # The two stations are local, so let's create the ticket and execute the TICKET_READY Operation (synchronous case)
			Environment.Travelers(Traveler_Index).Ticket := Create_Ticket(
				From 	=> From,
				To		=> To);

			Task_Pool.Execute(Environment.Operations(Traveler_Index)(Traveler.TICKET_READY));

		else
			-- # If no local resolution of the ticket can be done, make a request to the Central Ticket Office (asynchronous case)
			Central_Office_Interface.Ask_For_Ticket(
				From		 	=> From,
				To 				=> To,
				Traveler_Index	=> Traveler_Index);
		end if;
    end Get_Ticket;


	procedure Buy_Ticket(
		This			: in 	Station_Ticket_Office;
		Traveler_Index 	: in 	Positive;
		From			: in 	String;
		To				: in 	String)
	is
	begin
		-- # Simply invokes Get_Ticket procedure
		Get_Ticket(Traveler_Index,From,To);
    end Buy_Ticket;

end Ticket_Office;