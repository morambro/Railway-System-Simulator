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
with Train;use Train;
with Queue;
with Traveler;use Traveler;


package Platform is

	-- Create a queue for Traveler type
	package Traveler_Queue_Package is new Queue(Element => Positive);

	-- Queue for Arriving Traveler
	Arrival_Queue : Traveler_Queue_Package.Unbounded_Queue.Queue;

	-- Queue for Travelers waiting for the train
	Leaving_Queue : Traveler_Queue_Package.Unbounded_Queue.Queue;

	protected type Platform_Type(ID:Integer) is

		entry Enter(T : Train_Descriptor);

		procedure Leave(Descriptor : in out Train_Descriptor);

		procedure Add_Incoming_Traveler(Traveler : Positive);

		procedure Add_Outgoing_Traveler(Traveler : Positive);

	private
		Free : Boolean := True;
	end Platform_Type;

end Platform;
