with Generic_Station;use Generic_Station;
with Train;
with Plattform;
with Traveler;
with Notice_Panel;

package Regional_Station is

	-- Array Containing plattforms references
	type Plattforms_List is array (Positive range <>) of access Plattform.Plattform_Type;

	-- Definition of Regional Station Type implementing Station_Interface --
	type Regional_Station_Type(Plattforms_Number : Positive) is new Station_Interface with private;

		overriding procedure Enter(
			This : Regional_Station_Type;
			Descriptor : in out Train.Train_Descriptor;
			Plattform : Integer);

		overriding procedure Leave(
			This : Regional_Station_Type;
			Descriptor : in out Train.Train_Descriptor;
			Plattform : Integer);

		overriding procedure WaitForTrain(
			This : Regional_Station_Type;
			Incoming_Traveler : in out Traveler.Traveler_Manager;
			Plattform : Integer);

	function NewRegionalStation(
		Plattforms_Number : Positive;
		Name : Positive) return Station_Ref;

private

	type Regional_Station_Type(Plattforms_Number : Positive) is new Station_Interface with
	record
		Name : Positive;
		Plattforms : Plattforms_List(1..Plattforms_Number);
		Panel : access Notice_Panel.Notice_Panel_Entity;
	end record;

end Regional_Station;
