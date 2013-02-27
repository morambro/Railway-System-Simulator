package body Route is

	function GetNextTrack (S : Stage) return Positive is
	begin
		return S.Next_Track;
	end GetNextTrack;
	
	function GetNextStation (S : Stage) return Positive is
	begin
		return S.Next_Station;
	end GetNextStation;
	
	function GetTimeToLeave (S : Stage) return Time is
	begin
		return S.Leave_At;
	end GetTimeToLeave;
	
	function NewStage(T : Positive;S : Positive;Leave_At : Time) return Stage is
		To_Return : Stage;
	begin
		To_Return.Next_Track := T;
		To_Return.Next_Station := S;
		To_Return.Leave_At := Leave_At;
		return To_Return;
	end NewStage;
	
end Route;
