--==============================================================================
-- File:
--	logger.adb
-- Author:
--	Moreno Ambrosin
--	Mat.  : 1035635
-- Date:
-- 	27/02/2013
--==============================================================================

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
					Put_Line(Message);
					Put(ASCII.ESC & "[00m");
				when DEBUG 	=>
					Put("[D] ");
					Put_Line(Sender & " : " & Message);
	    	end case;
		end if;
		end Log;
    end Logger_Entity;

end Logger;