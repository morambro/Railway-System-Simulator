--==============================================================================
-- File:
--	logger.ads
-- Author:
--	Moreno Ambrosin
--	Mat.  : 1035635
-- Date:
-- 	27/02/2013
--==============================================================================

package Logger is

	type Log_Level is (INFO,NOTICE,DEBUG);

	function Init(L : String) return Boolean;

	procedure Log(Sender : String; Message : String; L : Log_Level);

private

	function ToLevel(L : String) return Log_Level;

	Level 		: Log_Level;

	Initiated 	: Boolean := false;

	Wrong_Input : exception;

end Logger;