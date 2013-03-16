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

with Unchecked_Deallocation;

-- Package which defines an Interface to expose a unique method, to be invoked
-- by worker tasks.
package Generic_Operation_Interface is

	-- Operation Interface type declaration
	type Operation_Interface is abstract tagged null record;

	-- Operation Method declaration
	procedure Do_Operation (X : in Operation_Interface) is abstract;

   	-- Operation reference type to be used inside records: Type'Class doesn't
   	-- have a fixed size so it can be not allocated inside a record.
   	type Any_Operation is access all Operation_Interface'Class;


   		type Traveler_Operations is Array(Positive range <>) of Any_Operation;

	type Traveler_Operations_Ref is access all Traveler_Operations;

	type Travelers_All_Operations is array (Positive range <>) of Traveler_Operations_Ref;

	type Travelers_All_Operations_Ref is access all Travelers_All_Operations;


	-- Code to manage memory deallocation
 	procedure Free is new Unchecked_Deallocation (
      	Operation_Interface'Class,
		Any_Operation
	);

   	pragma Controlled (Any_Operation);

end Generic_Operation_Interface;
