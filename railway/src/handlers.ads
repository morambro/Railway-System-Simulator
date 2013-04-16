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
with YAMI.Parameters;
with YAMI.Incoming_Messages;use YAMI.Incoming_Messages;
with Environment;
with Trains;
with Train;
with Train_Pool;
with Logger;
with Routes;

package Handlers is

	-- #
	-- # Handler used to Handle Messages to transfer Trains between Gateway Stations
	-- #
	procedure Station_Train_Transfer_Handler(
		Msg : in 	Incoming_Message'Class);

	-- #
	-- # This Handler will be used to Free the Platform at the sender side, when a remote Train transfer is made.
	-- #
	procedure Station_Train_Transfer_Ack_Handler(
		Msg : in 	Incoming_Message'Class);

	-- #
	-- # This handler is used by the message agent to handle messages of traveler remote transfer; the case is
	-- # the traveler next stage is on a different node, so is transfered to the proper Gateway Station,
	-- #
	procedure Station_Traveler_Leave_Transfer_Handler(
		Msg : in 	Incoming_Message'Class);

	-- #
	-- # This handler is used by the message agent to handle messages of traveler remote transfer, entering a platform
	-- #
	-- #
	procedure Station_Traveler_Enter_Transfer_Handler(
		Msg : in 	Incoming_Message'Class);

	-- #
	-- # Handles ticket creation requests from the Central Ticket Office, handling messages of type [ticket_creation].
	-- #
	procedure Get_Ticket_Handler(
		Msg : in 	Incoming_Message'Class);

	-- #
	-- # Handler Called in response of a message of type [is_present], to tell the Central Ticket Office if the
	-- # Given Station is present or not on this Region.
	-- #
    procedure Is_Station_Present_Handler(
		Msg : in 	Incoming_Message'Class);

end Handlers;
