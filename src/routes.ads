with Route; use Route;
with Ada.Real_Time;

package Routes is

    Route : Route_Type(1..3) := (
		1 => Newstage(Track => 1,Station => 2,Next_Plattform => 1,Leave_At => Ada.Real_Time.Clock),
		2 => Newstage(Track => 2,Station => 3,Next_Plattform => 1,Leave_At => Ada.Real_Time.Clock),
		3 => Newstage(Track => 3,Station => 4,Next_Plattform => 1,Leave_At => Ada.Real_Time.Clock)
	);

end Routes;