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
--  Railway_Simulation is distributed in the hope that it will be useful,			--
--  but WITHOUT ANY WARRANTY; without even the implied warranty of				--
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the				--
--  GNU General Public License for more details.								--
--																				--
--  You should have received a copy of the GNU General Public License			--
--  along with Railway_Simulation.  If not, see <http://www.gnu.org/licenses/>.		--
----------------------------------------------------------------------------------

with Ada.Text_IO;use Ada.Text_IO;
with Ada.Strings.Unbounded;
with Environment;

package body Move_Operation is

	use Ada.Strings.Unbounded;

	procedure Do_Operation(This : in Move_Operation_Type) is
	begin

		Put_Line("Move Operation for Passenger " & To_String(This.Manager.Traveler.Name));

		Environment.Get_Stations(1).WaitForTrain(This.Manager.all,2);

		-- Points to the next Operation to
		This.Manager.Next_Operation := This.Manager.Next_Operation + 1;
		N := N + 1;
		Put_Line("Added to queue");
	end Do_Operation;

	function NewOperation(T_Manager : access Traveler_Manager) return Any_Operation is
	begin
		return new Move_Operation_Type;
	end;

end Move_Operation;

