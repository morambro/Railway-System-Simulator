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


package body Environment Is

	-- Creation of Regional Stations
	Stations 	: Generic_Station.Stations_array_Ref := null;

	-- array of Travelers
    Travelers 	: Traveler.Traveler_Manager_Array_Ref := null;

--      Operations 	: array (1 .. 4) of Traveler.Traveler_Operations(1 .. 2) := (
--  		-- Operations for Traveler1
--  		1 =>  (	1 => new Move_Operation.Move_Operation_Type'(Manager => Travelers(1)'Access),
--  		        2 => new Move_Operation.Move_Operation_Type'(Manager => Travelers(1)'Access)),
--  		2 =>  (	1 => new Move_Operation.Move_Operation_Type'(Manager => Travelers(2)'Access),
--  		        2 => new Move_Operation.Move_Operation_Type'(Manager => Travelers(2)'Access)),
--  		3 =>  (	1 => new Move_Operation.Move_Operation_Type'(Manager => Travelers(3)'Access),
--  		        2 => new Move_Operation.Move_Operation_Type'(Manager => Travelers(3)'Access)),
--  		4 =>  (	1 => new Move_Operation.Move_Operation_Type'(Manager => Travelers(4)'Access),
--  		        2 => new Move_Operation.Move_Operation_Type'(Manager => Travelers(4)'Access))
--      );

	function Get_Travelers return Traveler.Traveler_Manager_Array_Ref is
	begin
		return Travelers;
    end Get_Travelers;

    function Get_Stations return Generic_Station.Stations_array_Ref is
    begin
    	return Stations;
    end Get_Stations;

    procedure Init is
    begin
    	Stations 	:= Regional_Station.Get_Regional_Station_Array("res/stations.json");
    	Travelers 	:= Traveler.Get_Traveler_Manager_array("res/travelers.json");
    end Init;

end Environment;
