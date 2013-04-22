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
-- #
-- # Represents the Remote Interface to communicate with other Nodes.
-- #
package Remote_Station_Interface is

	Stop_Train_Execution : Exception;

	-- #
	-- # Procedure used to Transfer a Train via remote message to a given destination.
	-- #
	procedure Send_Train(
		Train_Descriptor_Index 	: in	 Positive;
		Station	 				: in	 Positive;
		Platform				: in 	 Positive;
		Next_Node_Name			: in 	 String);


	-- #
	-- # Procedure used to Send an Acknowledge message to notify the sender Gateway Station
	-- # that the sent Train left the destination Gateway Station.
	-- #
	procedure Send_Ack(
		Train_Descriptor_Index 	: in	 Positive;
		Station	 				: in	 Positive;
		Platform				: in 	 Positive;
		Node_Name				: in	 String);

	-- #
	-- # Procedure used to transfer a Traveler from a Gateway Station to the respective on node [Node_Name],
	-- # to let him wait on the correct platform to catch another Train.
	-- #
	procedure Send_Traveler_To_Leave(
		Traveler_Index	: in	 Positive;
		Train_ID		: in 	 Positive;
		Station	 		: in	 Positive;
		Platform		: in 	 Positive;
		Node_Name		: in	 String );

	-- #
	-- # Remote Procedure used to make a Traveler wait to arrive to a Station on a different Region.
	-- #
	procedure Wait_For_Train_To_Arrive(
		Next_Station 				: in 	 Positive;
		Traveler_Manager_Index		: in 	 Positive;
		Train_ID					: in 	 Positive;
		Destination_Platform_Index	: in 	 Positive;
		Next_Region					: in 	 String);


end Remote_Station_Interface;
