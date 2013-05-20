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
with Ada.Strings.Unbounded;use  Ada.Strings.Unbounded;
with Environment;
with Train;use Train;
with Trains;

package body Routes is

	procedure Init is
	begin
		All_Routes := Route.Get_Routes("../configuration/routes.json");
    end Init;

	function Contains(
		Route_Index : in 	Positive;
		From		: in 	Positive;
		To			: in 	Positive) return Natural
	is
		Found 	: Natural := 0;
		I 		: Positive := 1;
	begin
		while (I <= All_Routes(Route_Index)'Length) and (Found = 0) loop
			-- # If current route's stage can bring from [From] to [To], return it.
			if 	(All_Routes(Route_Index)(I).Start_Station 		= From) 	and
				(All_Routes(Route_Index)(I).Next_Station 		= To) 		and
				(All_Routes(Route_Index)(I).Leave_Action 		= ENTER) 	and
				(To_String(All_Routes(Route_Index)(I).Node_Name)= Environment.Get_Node_Name)
			then
				Found := I;
			end if;
			I := I + 1;
		end loop;
		return Found;
    end Contains;


	function Get_Routes_Containing(
		From		: in 	Positive;
		To			: in 	Positive) return Routes_Indexes
	is
		Size 			: Natural := 0;
		Routes_Found	: Routes_Indexes(1..All_Routes'Length);
	begin
		for I in 1 .. All_Routes'Length loop
			if Contains(I,From,To) /= 0 then
				Size := Size + 1;
				Routes_Found(Size) := I;
			end if;
		end loop;
		return Routes_Found(1..Size);
	end;

end Routes;
