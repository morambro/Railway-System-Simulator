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

with Generic_Operation_Interface;use Generic_Operation_Interface;
with Queue;

package Task_Pool is

	-- Adds a new operation to the executing queue
	procedure Execute(Operation : Any_Operation);

	-- Task which Executes an operation taken from Operations_Queue
	task type Actor;

	type Actors_Vector is array(Integer range <>) of Actor;

	type Task_Pool_Type(Pool_Dimension : Positive) is record
		Actors : Actors_Vector(1..Pool_Dimension);
	end record;

private

	-- Declaration of a new Queue package with Element => Any_Operation
	package Operations_Queue_Package is new Queue(Element => Any_Operation);

	-- Queue used to manage Traveler operations
	Operations_Queue : Operations_Queue_Package.Unbounded_Queue.Queue;


end Task_Pool;
