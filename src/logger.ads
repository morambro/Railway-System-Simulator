--==============================================================================
-- File:
--	logger.ads
-- Author:
--	Moreno Ambrosin
--	Mat.  : 1035635
-- Date:
-- 	27/02/2013
--==============================================================================

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