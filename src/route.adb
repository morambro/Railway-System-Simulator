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
    End Gettimetoleave;

    function GetNextPlattform (S : Stage) return Positive is
    begin
		return S.Plattform_Index;
    end GetNextPlattform;

    --
    -- Creates a new Stage with the given parameters and returns it.
    --
    function Newstage(
		      Track : Positive;
		      Station : Positive;
		      Next_Plattform : Positive;
		      Leave_At : Time)
    return Stage is
		To_Return : Stage;
	begin
		To_Return.Next_Track        := Track;
	    To_Return.Next_Station      := Station;
	    To_Return.Plattform_Index   := Next_Plattform;
		To_Return.Leave_At          := Leave_At;
		return To_Return;
	end NewStage;

end Route;
