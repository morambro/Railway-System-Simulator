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
with Ticket;use Ticket;
with Time_Table;

package Central_Office_Interface is

	procedure Ask_For_Ticket(
			From			: String;
			To				: String;
			Traveler_Index	: Positive);

	procedure Validate(
			The_Ticket 		: Ticket.Ticket_Type_Ref;
			Callback 		: access procedure(The_Ticket:Ticket.Ticket_Type_Ref;Response: Boolean));

	procedure Update_Run(
			Route_Index 	: Positive;
			Current_Run		: Positive;
			Callback 		: access procedure(
								Updated			: in 	 Boolean;
								New_Time_Table 	: access Time_Table.Time_Table_Type));

	procedure Load_Time_Tables(
			Callback 		: access procedure (Table : in	Time_Table.Time_Table_Array_Ref));

end Central_Office_Interface;