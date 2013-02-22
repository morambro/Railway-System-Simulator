package body Route is
	
	function NewStage(T : Positive;S : Positive;Leave_At : Integer) return Stage is
		To_Return : Stage;
	begin
		To_Return.Next_Track := T;
		To_Return.Next_Station := S;
		To_Return.Leave_At := Leave_At;
		return To_Return;
	end NewStage;
	
end Route;
