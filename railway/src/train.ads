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

package Train is

	type Train_Type is (FB,REGIONAL);

	type Train_Descriptor is
	record
	    Id 			    : Positive;
		Speed 		    : Positive;
	    Max_Speed 	    : Positive;
	    -- Id of the current station
	    Current_Station : Positive;
	    -- Id of the next stage;
	    Next_Stage		: Positive;
	    -- Index of the Route
	    Route_Index		: Positive;
	    -- # Number of sits
	    Sists_Number 	: Natural;
	    -- # Number of Travelers Currently in the Train
	    Occupied_Sits 	: Natural := 0;
		-- # The train type
		T_Type			: Train_Type;

	End Record;

	type Trains_Array is array (Positive range <>) of Train_Descriptor;

	----------------- Procedures to convert Json to Train_Descriptor ----------------------------

	function Get_Trains_Array (
		Json_File_Name 	: in String) return Trains_Array;

	procedure Print(T : access Train_Descriptor);

private

	function Get_Train_Descriptor (
		Json_Train 		: in Json_Value) return Train_Descriptor;

	function String_To_Train_Type (
		T_Type			: in String) return Train_Type;


end Train;
