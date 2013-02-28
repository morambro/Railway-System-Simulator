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

	--
	-- Logger initialization
	--
	function Init(L : String) return Boolean is
	begin
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
		if(L <= Level) then
			Put_Line(Sender & " : " & Message);
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

end Logger;