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
with Ada.Text_IO;
with JSON_Helper;

package body Environment Is

	Regional_Stations 	: Generic_Station.Stations_array_Ref := null;

	-- array of Travelers
    Travelers 			: Traveler.Traveler_Manager_Array_Ref := null;

	Operations 			: Traveler.Travelers_All_Operations_Ref := null;

	function Get_Travelers return Traveler.Traveler_Manager_Array_Ref is
	begin
		return Travelers;
    end Get_Travelers;

    function Get_Regional_Stations return Generic_Station.Stations_array_Ref is
    begin
    	return Regional_Stations;
    end Get_Regional_Stations;

    function Get_Operations return Traveler.Travelers_All_Operations_Ref is
    begin
    	return Operations;
    end Get_Operations;

    procedure Init is
    begin
    	-- # Creates regional stations array loading data from file
    	Regional_Stations 	:= Regional_Station.Get_Regional_Station_Array("res/stations.json");

		-- # Creates travelers array loading data from file
    	Travelers 	:= Traveler.Get_Traveler_Manager_array("res/travelers.json");

		Travelers(1).Ticket := Ticket.Get_Ticket(JSON_Helper.Load_File("res/tickets.json"));

		-- # Create an operations set for each Traveler
    	Operations	:= new Traveler.Travelers_All_Operations(1 .. Travelers'Length);

		for I in 1 .. Operations'Length loop
			Operations(I) := new Traveler.Traveler_Operations(Traveler.LEAVE .. Traveler.ENTER);
			Operations(I)(Traveler.LEAVE) := new Move_Operation.Leave_Operation_Type'(Traveler_Manager_Index => I);
			Operations(I)(Traveler.ENTER) := new Move_Operation.Enter_Operation_Type'(Traveler_Manager_Index => I);
		end loop;

    end Init;

end Environment;
