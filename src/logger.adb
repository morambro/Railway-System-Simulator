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

with Ada.Text_IO;use Ada.Text_IO;

package body Logger is

	-- 30=black 31=red 32=green 33=yellow 34=blue 35=magenta 36=cyan 37=white

	--
	-- Logger initialization
	--
	function Init(L : String) return Boolean is
	begin
		if Initiated then
			return true;
		end if;
		Level := ToLevel(L);
		Initiated := true;
		return true;
	exception
		when Wrong_Input =>
			Put_Line ("Logger, ERROR : Wrong Message type");
			return false;
	end Init;

	--
	-- Log procedure is used to print a given Message from a Sender, at a given
	-- log level
	--
	procedure Log(Sender : String; Message : String; L : Log_Level) is
	begin
		if Initiated then
			Logger_Entity.Log(Sender,Message,L);
    	end if;
    end Log;

	--
	-- Private function which converts a given string to a Log Level; if it is not
	-- possible, the function rises a Wrong_Input exception.
	--
	function ToLevel(L : String) return Log_Level is
	begin
		if (L = "-i") then
			return INFO;
		elsif (L = "-n") then
			return NOTICE;
		elsif (L = "-d") then
			return DEBUG;
		end if;
		raise Wrong_Input;
	end;


	protected body Logger_Entity is
		procedure Log(Sender : String; Message : String; L : Log_Level) is
		begin
			if(L <= Level) then
			Put("[" & Ada.Calendar.Formatting.Image(Ada.Calendar.Clock) & "] ");
			case L is
				when INFO =>
					Put("[I] ");
					Put(ASCII.ESC & "[32m");
					Put_Line(Message);
					Put(ASCII.ESC & "[00m");
				when NOTICE =>
					Put("[N] ");
					Put(ASCII.ESC & "[34m");
					Put_Line(Message);
					Put(ASCII.ESC & "[00m");
				when ERROR =>
					Put("[E] ");
					Put(ASCII.ESC & "[31m");
					Put_Line(Sender & " : " & Message);
					Put(ASCII.ESC & "[00m");
				when DEBUG 	=>
					Put("[D] ");
					Put_Line(Sender & " : " & Message);
	    	end case;
		end if;
		end Log;
    end Logger_Entity;

end Logger;
