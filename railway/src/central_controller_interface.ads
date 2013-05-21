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
with Message_Agent;
with YAMI.Parameters;

-- #
-- # This package provides a simple interface to let the system communicate with
-- # the Central Controller, via small messages.
-- #
package Central_Controller_Interface is

	-- ############################################# TRAIN ####################################

	-- #
	-- # Procedure used to send the one-way message to the Controller.
	-- #
	procedure Send_Event(Json_Event : String);

	-- #
	-- # Sends an event telling the Controller that the Train [Train] left station [Station]
	-- # platform [Platform], and accesses segment [Segment], running for [Time] seconds.
	-- #
    procedure Set_Train_Left_Status(
		Train		: Positive;
		Station		: String;
		Time		: Positive;
		Segment		: Positive);

	-- #
	-- # Sends an event telling that the Train [Train_ID] is arriving to
	-- # Station [Station] at platform [Platform], and is going to
	-- # do the action [Action].
	-- #
	procedure Set_Train_Arriving_Status(
		Station		: in	String;
		Train_ID	: in 	Integer;
		Platform	: in 	Integer;
		Time		: in 	String;
		Train_Delay	: in 	Integer);


	-- #
	-- # Sends an Event telling the Controller that the Train [Train_ID] arrived at station [Station]
	-- # platform [Platform], and will leave at [Time], with a delay of [Train_Delay]
	-- #
	procedure Set_Train_Arrived_Status(
		Train_ID 	: in 	Integer;
		Station		: in 	String;
		Segment 	: in 	Integer;
		Platform 	: in 	Integer;
		Time		: in 	String;
		Train_Delay	: in 	Integer);

	-- ############################################# TRAVELER ####################################

	-- #
	-- # Updates the status of the given traveler.
	-- #
	procedure Set_Traveler_Entering_Status(
		Traveler	: Positive;
		Train		: Positive;
		Station		: String;
		Platform	: Positive);

	-- #
	-- # Updates the status of the given traveler.
	-- #
	procedure Set_Traveler_Left_Status(
		Traveler	: Positive;
		Train		: Positive;
		Station		: String;
		Platform	: Positive);

	-- #
	-- # Updates the status of the given traveler.
	-- #
	procedure Set_Traveler_Finished_Status(
		Traveler	: Positive;
		Train		: Positive;
		Station		: String;
		Platform	: Positive);


	-- #
	-- # Updates the status of the given traveler.
	-- #
	procedure Set_Traveler_Buying_Status(
		Traveler	: Positive;
		Station		: String);

	-- #
	-- # This procedure is used to notify the termination of the current Node
	-- # to the Controller, to complete the termination procedure.
	-- #
	procedure Notify_Termination;

end Central_Controller_Interface;