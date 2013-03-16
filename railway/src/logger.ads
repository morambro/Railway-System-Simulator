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

with Ada.Calendar;
with Ada.Calendar.Formatting;

-- # This package provides a method to
package Logger is

	type Log_Level is (ERROR,INFO,NOTICE,DEBUG);

	function Init(L : String) return Boolean;

	procedure Log(Sender : String; Message : String; L : Log_Level);

private

	-- #
	-- # Private protected resource used to perform thread-safe Log output
	-- #
	protected Logger_Entity is
		procedure Log(Sender : String; Message : String; L : Log_Level);
    end Logger_Entity;

	-- #
	-- # Utility function which performs String -> Log_Level conversion
	-- #
	function ToLevel(L : String) return Log_Level;

	-- # tells weather the Logger have been initialized with the minimum log level to display
	Initiated 	: Boolean := false;

	-- # Minimum log level to display
	Level 		: Log_Level;

	-- # exception raised when a wrong input is received
	Wrong_Input : exception;

end Logger;
