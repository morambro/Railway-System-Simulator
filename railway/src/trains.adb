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
with Logger;

package body Trains is

	procedure Update_Train(
		Train_Index 	: in	Positive;
		Train_To_Copy	: in 	Train.Train_Descriptor)
	is
	begin

		if Train_Index <= Trains'Length then
			Trains(Train_Index).Speed 			:= Train_To_Copy.Speed;
			Trains(Train_Index).Max_Speed 		:= Train_To_Copy.Max_Speed;
	    	Trains(Train_Index).Current_Station := Train_To_Copy.Current_Station;
	    	Trains(Train_Index).Next_Stage 		:= Train_To_Copy.Next_Stage;
	    	Trains(Train_Index).Route_Index 	:= Train_To_Copy.Route_Index;
	    	Trains(Train_Index).Sits_Number 	:= Train_To_Copy.Sits_Number;
	    	Trains(Train_Index).Occupied_Sits	:= Train_To_Copy.Occupied_Sits;
	    	Trains(Train_Index).T_Type			:= Train_To_Copy.T_Type;
		else
			Logger.Log(
				Sender 		=> "Trains",
				Message 	=> "ERROR: Error while updating Train data. The given Train is not the same as Train " & Integer'Image(Train_Index),
				L 			=> Logger.ERROR);
		end if;

    end Update_Train;

    function Train_For_Route(
		Route_Index		: in 	Positive) return Natural
	is
	begin
		for I in 1 .. Trains'Length loop
			Ada.Text_IO.Put_Line(Integer'Image(Route_Index) & "  " & Integer'Image(Trains(I).Route_Index));
			if Route_Index = Trains(I).Route_Index then
				return Trains(I).ID;
			end if;
		end loop;
		return 0;
    end Train_For_Route;


   	function Get_Train_Index(
   		Train_ID		: in 	Positive) return Natural
   	is
   	begin
   		for I in 1 .. Trains'Length loop
   			if Trains(I).Id = Train_ID then
   				return I;
   			end if;
   		end loop;
   		return 0;
    end Get_Train_Index;

end Trains;
