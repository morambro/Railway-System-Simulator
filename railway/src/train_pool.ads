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

with Queue;
with Train;use Train;
with Ada.Numerics.Discrete_Random;
with System;use System;

-- #
-- # This Package contains a definition of a Train task pool; Each Task is responsible
-- # of doing certain operations defined in the task body, for a certain Train_Descriptor
-- #
package Train_Pool is


	type Priority is (LOW,HIGH);

	-- # Task used to execute Trains
	task type Train_Executor_Task(
		-- # Tells from which queue read
		Priority_Level 	:  	Priority)
	is
    end Train_Executor_Task;


	type Low_Priority_Vector is array (Positive range <>) of Train_Executor_Task(LOW);


	type High_Priority_Vector is array (Positive range <>) of Train_Executor_Task(HIGH);


	type Train_Task_Pool(
		Low_Priority_Pool_Size : Positive;
		High_Priority_Pool_Size : Positive) is limited private;

	-- #
	-- # Procedure used to put the given train descriptor index in the right queue,
	-- # to let the train move.
	-- #
	procedure Associate(Train_D : Positive);

	procedure Stop;

private

	-- #
	-- # Declaration of a new Queue package with Element => Train_Descriptor
	-- #
--  	package Trains_Queue_Package is new Queue(Element => Train_Descriptor);
	package Trains_Queue_Package is new Queue(Element => Positive);

	-- #
	-- # Queue used to manage Traveler operations
	-- #
	Low_Priority_Trains_Queue 	: Trains_Queue_Package.Terminable_Queue;
	High_Priority_Trains_Queue 	: Trains_Queue_Package.Terminable_Queue;

	-- #
	-- # Private Definition of Train_Task_Pool type as a record with two pools of
	-- # tasks, one low priority queue, and one at higher priority
	-- #
	type Train_Task_Pool(
		Low_Priority_Pool_Size 	: Positive;
		High_Priority_Pool_Size : Positive) is record

		Low_Tasks 	: Low_Priority_Vector(1 .. Low_Priority_Pool_Size);
		High_Tasks 	: High_Priority_Vector(1 .. High_Priority_Pool_Size);

	end record;

end Train_Pool;
